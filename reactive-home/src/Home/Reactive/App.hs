{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Home.Reactive.App (
  defaultMainWith,
  cliOptsP,
  CLIOpts (..),
  Config (..),
  MackerelConfig (..),
  defaultMain,
) where

import Control.Applicative ((<**>))
import Control.Exception (throwIO)
import Control.Monad (forM_)
import Control.Monad.Trans.Class (lift)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (defaultTimeLocale, getZonedTime)
import Data.Time.Format (formatTime)
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Console.ByteString (Console, runConsole)
import Effectful.Console.ByteString qualified as Console
import Effectful.Console.ByteString qualified as Eff
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Network.Mqtt
import Effectful.Reader.Static (Reader, asks, runReader)
import Effectful.Wreq (Wreq, runWreq)
import FRP.Rhine
import GHC.Generics (Generic)
import Home.Reactive.App.Types (ParseResult (..))
import Home.Reactive.ESPresense
import Home.Reactive.MQTT
import Home.Reactive.Metrics.Mackerel
import Home.Reactive.Orphans ()
import Home.Reactive.Sesame5
import Options.Applicative qualified as Opts
import Toml hiding (first, map)

data Config = Config
  { host :: !T.Text
  , port :: !Int
  , clientId :: !(Maybe T.Text)
  , user :: !(Maybe T.Text)
  , password :: !(Maybe T.Text)
  , espresense :: !(Maybe ESPresenseConfig)
  , sesame :: !(Maybe SesameConfig)
  , mackerel :: !(Maybe MackerelConfig)
  , logLevel :: !(Maybe LogLevel)
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (HasCodec) via TomlTable Config

data LogLevel = Debug | Info | Warning | Error
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)

instance HasCodec LogLevel where
  hasCodec = enumBounded

data CLIOpts = CLIOpts {configFile :: !FilePath}
  deriving (Show, Eq, Ord, Generic)

cliOptsP :: Opts.ParserInfo CLIOpts
cliOptsP =
  Opts.info (p <**> Opts.helper) $
    Opts.fullDesc <> Opts.progDesc "Debug app" <> Opts.header "reactive-home"
  where
    p = do
      configFile <-
        Opts.strOption $
          Opts.long "config"
            <> Opts.short 'c'
            <> Opts.metavar "FILE"
            <> Opts.value "config.toml"
            <> Opts.showDefault
            <> Opts.help "Path to the configuration file (TOML format)"
      pure CLIOpts {..}

configCodec :: TomlCodec Config
configCodec = genericCodec

data HomeEnv = HomeEnv
  { mqtt :: {-# UNPACK #-} !MqttClient
  , sesame :: {-# UNPACK #-} !(Maybe SesameEnv)
  , espresense :: !(Maybe ESPresenseConfig)
  , mackerel :: !(Maybe MackerelConfig)
  , logLevel :: !LogLevel
  }
  deriving (Generic)

reportErrors ::
  (Show e, Console :> es) =>
  ClSF (Eff es) cl (ParseResult e a) (Maybe a)
reportErrors = arrM \case
  ParseFailure err -> do
    lift $ Eff.putStrLn $ TE.encodeUtf8 $ T.pack $ "Error: " <> show err
    pure Nothing
  ParseSuccess a -> pure (Just a)
  Skipped -> pure Nothing

processSesame ::
  ( Reader HomeEnv :> es
  , Console :> es
  ) =>
  ClSF (Eff es) EffMqttClock Message (Maybe SesameStatus)
processSesame = proc msg -> do
  msess <- constMCl (asks @HomeEnv (.sesame)) -< ()
  parsed <- sesameStatuses -< (msess, msg)
  reported <- reportErrors -< parsed
  aggregateSesameStatus -< reported
  returnA -< reported

processESP ::
  (Console :> es) =>
  ClSF (Eff es) EffMqttClock Message (Maybe ESPStatus)
processESP = proc msg -> do
  reportErrors <-< parseESPStatusS -< msg

data MqttOutputs = MqttOutputs
  { mqttESPStatus :: !(Maybe ESPStatus)
  , mqttMackerelMetrics :: ![MackerelMetrics]
  }
  deriving (Show, Eq, Ord, Generic)

data AppTick = AppTick
  { tickESP :: !(Heartbeated ESPStatus)
  , tickMackerelMetrics :: ![MackerelMetrics]
  }
  deriving (Show, Eq, Ord, Generic)

processMqtt ::
  (Reader HomeEnv :> es, Console :> es) =>
  ClSF (Eff es) EffMqttClock () MqttOutputs
processMqtt = proc () -> do
  msg <- tagS -< ()
  arrMCl (display Debug) -< msg
  ssm <- arr toMetrics <-< processSesame -< msg
  esp <- processESP -< msg
  returnA
    -<
      MqttOutputs
        { mqttESPStatus = esp
        , mqttMackerelMetrics = ssm <> toMetrics esp
        }

withESPConfig ::
  (Reader HomeEnv :> es) =>
  Eff (Reader ESPresenseConfig : es) c -> Eff es c
withESPConfig action = do
  cfg <- asks @HomeEnv (.espresense)
  case cfg of
    Nothing -> error "ESPConfig not found in environment"
    Just espCfg -> runReader espCfg action

type MackerelClock es = IOClock (Eff es) (Millisecond 1200)

type ESPHeartbeatClock es = IOClock (Eff es) (Millisecond 100)

type AppClock es = SeqClock EffMqttClock (ParClock (ESPHeartbeatClock es) (MackerelClock es))

data AppBufferState = AppBufferState
  { bufferedESP :: !(Seq ESPStatus)
  , bufferedMackerel :: ![MackerelMetrics]
  }
  deriving (Show, Eq, Ord, Generic)

appBuffer ::
  ResamplingBuffer
    (Eff es)
    EffMqttClock
    (ParClock (ESPHeartbeatClock es) (MackerelClock es))
    MqttOutputs
    AppTick
appBuffer =
  ResamplingBuffer
    { buffer =
        AppBufferState
          { bufferedESP = Seq.empty
          , bufferedMackerel = []
          }
    , put = \_ MqttOutputs {..} state ->
        pure
          state
            { bufferedESP = maybe state.bufferedESP (state.bufferedESP Seq.|>) mqttESPStatus
            , bufferedMackerel = state.bufferedMackerel <> mqttMackerelMetrics
            }
    , get = \TimeInfo {tag} state ->
        case tag of
          Left _ ->
            case Seq.viewl state.bufferedESP of
              espStatus Seq.:< rest ->
                pure $
                  Result
                    state {bufferedESP = rest}
                    AppTick
                      { tickESP = Event espStatus
                      , tickMackerelMetrics = []
                      }
              Seq.EmptyL ->
                pure $
                  Result
                    state
                    AppTick
                      { tickESP = Heartbeat
                      , tickMackerelMetrics = []
                      }
          Right _ ->
            pure $
              Result
                state {bufferedMackerel = []}
                AppTick
                  { tickESP = Heartbeat
                  , tickMackerelMetrics = state.bufferedMackerel
                  }
    }

processESPHeartbeat ::
  ( Reader HomeEnv :> es
  , Console :> es
  ) =>
  ClSF (Eff es) (ESPHeartbeatClock es) AppTick ()
processESPHeartbeat = proc tick -> do
  mcfg <- constMCl (asks @HomeEnv (.espresense)) -< ()
  case mcfg of
    Nothing -> returnA -< ()
    Just _ -> do
      snapshot <- hoistClSF withESPConfig espresenseSnapshotS -< tick.tickESP
      arrMCl (display Debug) -< snapshot

bulkMackerelS ::
  ( Wreq :> es
  , Reader HomeEnv :> es
  , Concurrent :> es
  , Console :> es
  ) =>
  ClSF (Eff es) (MackerelClock es) AppTick ()
bulkMackerelS = proc stts -> do
  mcfg <- constMCl (asks @HomeEnv (.mackerel)) -< ()
  case mcfg of
    Nothing -> returnA -< ()
    Just cfg -> arrMCl (uncurry postMackerel) -< (cfg, stts.tickMackerelMetrics)

mainLoop ::
  ( Reader HomeEnv :> es
  , Mqtt :> es
  , Console :> es
  , Wreq :> es
  , Concurrent :> es
  , IOE :> es
  ) =>
  Rhine (Eff es) (AppClock es) () ()
mainLoop =
  processMqtt
    @@ EffMqttClock
    >-- appBuffer
    --> ( processESPHeartbeat
            @@ ioClock waitClock
            |@| bulkMackerelS
              @@ ioClock waitClock
        )

display :: (Reader HomeEnv :> es, Console :> es, Show a) => LogLevel -> a -> Eff es ()
display level a = do
  minLevel <- asks @HomeEnv (.logLevel)
  if level >= minLevel
    then do
      now <- unsafeEff_ getZonedTime
      let timestamp = formatTime defaultTimeLocale "[%Y-%m-%d %H:%M:%S %Z]" now
      Console.putStrLn . TE.encodeUtf8 . T.pack . (timestamp <>) . show $ a
    else pure ()

application ::
  ( Concurrent :> es
  , Reader HomeEnv :> es
  , Console :> es
  , Wreq :> es
  , IOE :> es
  , Mqtt :> es
  ) =>
  Eff es ()
application = do
  initializeESPresense
  flow mainLoop

initializeESPresense :: (Mqtt :> es, Reader HomeEnv :> es) => Eff es ()
initializeESPresense = do
  cfg <- asks @HomeEnv (.espresense)
  forM_ cfg $ \espCfg ->
    initialiseRooms espCfg.devices espCfg.sensors

defaultMainWith :: Config -> IO ()
defaultMainWith config = do
  let !topics =
        foldMap espresenseTopicFilters config.espresense
          <> foldMap sesameTopicFilters config.sesame
  case NE.nonEmpty topics of
    Nothing -> putStrLn "No topics to subscribe to; exiting."
    Just ts -> do
      let !mqttCfg =
            MqttClockConfig
              { host = T.unpack $ host config
              , port = port config
              , user = user config
              , password = password config
              , clientId = fromMaybe "" config.clientId
              , subscriptions =
                  ts <&> \topic ->
                    Subscription
                      { topicFilter = topic
                      , retainHandling = SendOnSubscribe
                      , retainAsPublished = False
                      , noLocal = True
                      , qos = QoS1
                      }
              }
      let sesame = fromSesameConfig <$> config.sesame
          espresense = config.espresense
          mackerel = config.mackerel
          logLevel = fromMaybe Info config.logLevel
      withMqttClient mqttCfg \mqtt sess ->
        runEff $
          runConsole $
            runWreq $
              runReader HomeEnv {..} $
                runMqttWith mqtt sess $
                  runConcurrent application

defaultMain :: IO ()
defaultMain = do
  CLIOpts {..} <- Opts.execParser cliOptsP
  config <- either (throwIO . userError . show) pure =<< decodeFileExact configCodec configFile
  defaultMainWith config
