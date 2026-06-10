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
  application,
  cliOptsP,
  CLIOpts (..),
  Config (..),
  MackerelConfig (..),
  defaultMain,
) where

import Control.Applicative ((<**>))
import Control.Exception (throwIO)
import Control.Exception.Safe (SomeException, handleAny)
import Control.Exception.Safe qualified as E
import Control.Lens ((&), (.~))
import Control.Monad (forM_)
import Control.Monad qualified as M
import Control.Monad.Fix (fix)
import Control.Monad.Trans.Class (lift)
import Data.Aeson qualified as A
import Data.Functor (void, (<&>))
import Data.Generics.Labels ()
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (defaultTimeLocale, getZonedTime)
import Data.Time.Format (formatTime)
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent, threadDelay)
import Effectful.Concurrent.Async (concurrently_)
import Effectful.Concurrent.STM (TQueue, atomically, flushTQueue, newTQueueIO, writeTQueue)
import Effectful.Console.ByteString (Console, runConsole)
import Effectful.Console.ByteString qualified as Console
import Effectful.Console.ByteString qualified as Eff
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Network.Mqtt
import Effectful.Reader.Static (Reader, asks, runReader)
import Effectful.Wreq (Wreq, runWreq)
import Effectful.Wreq qualified as W
import FRP.Rhine
import GHC.Generics (Generic)
import Home.Reactive.App.Types (ParseResult (..))
import Home.Reactive.ESPresense
import Home.Reactive.MQTT
import Home.Reactive.Metrics.Mackerel
import Home.Reactive.Orphans ()
import Home.Reactive.Sesame5
import Home.Reactive.Unlock
import Options.Applicative qualified as Opts
import System.Random (randomRIO)
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
  , unlock :: !(Maybe UnlockConfig)
  , logLevel :: !(Maybe LogLevel)
  , mqtt :: !(Maybe MqttDevices)
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
  , mqttDevices :: !(Maybe MqttDevices)
  , sesame :: {-# UNPACK #-} !(Maybe SesameEnv)
  , espresense :: !(Maybe ESPresenseConfig)
  , mackerel :: !(Maybe MackerelConfig)
  , mackerelMetricsQueue :: !(TQueue MackerelMetrics)
  , unlock :: !(Maybe UnlockConfig)
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
  , mqttSnapshot :: !MqttSnapshot
  }
  deriving (Show, Eq, Ord, Generic)

data AppTick = AppTick
  { tickESP :: !(Heartbeated ESPStatus)
  , tickMqttSnapshot :: !MqttSnapshot
  }
  deriving (Show, Eq, Ord, Generic)

emptyMqttSnapshot :: MqttSnapshot
emptyMqttSnapshot = MqttSnapshot {switches = HM.empty}

processMqtt ::
  (Reader HomeEnv :> es, Console :> es, Concurrent :> es) =>
  ClSF (Eff es) EffMqttClock () MqttOutputs
processMqtt = proc () -> do
  msg <- tagS -< ()
  arrMCl (display Debug . T.show) -< msg
  ssm <- arr toMetrics <-< processSesame -< msg
  esp <- processESP -< msg
  arrMCl enqueueMackerelMetrics -< ssm <> toMetrics esp
  devices <- constMCl (asks @HomeEnv (.mqttDevices)) -< ()
  mqttSnapshot <- case devices of
    Nothing -> returnA -< emptyMqttSnapshot
    Just {} -> hoistClSF withMqttDevices mqttSnapshotS -< msg
  returnA
    -<
      MqttOutputs
        { mqttESPStatus = esp
        , mqttSnapshot
        }

enqueueMackerelMetrics :: (Reader HomeEnv :> es, Concurrent :> es) => [MackerelMetrics] -> Eff es ()
enqueueMackerelMetrics [] = pure ()
enqueueMackerelMetrics metrics = do
  mcfg <- asks @HomeEnv (.mackerel)
  forM_ mcfg $ \_ -> do
    queue <- asks @HomeEnv (.mackerelMetricsQueue)
    atomically $ mapM_ (writeTQueue queue) metrics

withMqttDevices ::
  (Reader HomeEnv :> es) =>
  Eff (Reader MqttDevices : es) c -> Eff es c
withMqttDevices action = do
  cfg <- asks @HomeEnv (.mqttDevices)
  case cfg of
    Nothing -> error "MqttDevices not found in environment"
    Just mqttDevices -> runReader mqttDevices action

withESPConfig ::
  (Reader HomeEnv :> es) =>
  Eff (Reader ESPresenseConfig : es) c -> Eff es c
withESPConfig action = do
  cfg <- asks @HomeEnv (.espresense)
  case cfg of
    Nothing -> error "ESPConfig not found in environment"
    Just espCfg -> runReader espCfg action

type ESPHeartbeatClock es = IOClock (Eff es) (Millisecond 500)

type AppClock es = SeqClock EffMqttClock (ESPHeartbeatClock es)

appBuffer ::
  ResamplingBuffer
    (Eff es)
    EffMqttClock
    (ESPHeartbeatClock es)
    MqttOutputs
    AppTick
appBuffer =
  arr (\MqttOutputs {..} -> (mqttESPStatus, mqttSnapshot))
    ^->> dropNothingBuffer fifoUnbounded
      *-* keepLast emptyMqttSnapshot
      >>-^ arr
        ( \(esp, mqttSnapshot) ->
            AppTick
              { tickESP = maybe Heartbeat Event esp
              , tickMqttSnapshot = mqttSnapshot
              }
        )

dropNothingBuffer ::
  (Monad m) =>
  ResamplingBuffer m clIn clOut a b ->
  ResamplingBuffer m clIn clOut (Maybe a) b
dropNothingBuffer ResamplingBuffer {..} =
  ResamplingBuffer
    { put = \inputTime value state ->
        case value of
          Nothing -> pure state
          Just a -> put inputTime a state
    , get
    , buffer
    }

processESPHeartbeat ::
  ( Reader HomeEnv :> es
  , Console :> es
  , Mqtt :> es
  ) =>
  ClSF (Eff es) (ESPHeartbeatClock es) AppTick ()
processESPHeartbeat = proc tick -> do
  mcfg <- constMCl (asks @HomeEnv (.espresense)) -< ()
  case mcfg of
    Nothing -> returnA -< ()
    Just _ -> do
      mdelta <- hoistClSF withESPConfig espresenseDeltaS -< tick.tickESP
      void $ mapMaybe (arrMCl $ display Debug . T.show) -< mdelta
      snapshot <- hoistClSF withESPConfig aggregateESPresenseDeltaS -< mdelta
      void $ arrMCl (display Debug . ("Mqtt Snapshot: " <>) . T.show) -< tick.tickMqttSnapshot
      void $ mapMaybe (arrMCl $ display Debug . T.show) -< snapshot <$ mdelta
      cfg <- constMCl (asks @HomeEnv (.unlock)) -< ()
      case cfg of
        Nothing -> returnA -< ()
        Just {} -> do
          -- FIXME: too dirty!
          (fb, result) <- hoistClSF withUnlockConfig unlockFeedbackS -< (tick.tickMqttSnapshot, snapshot)
          arrMCl (display Debug . ("ESP feedback: " <>) . T.show) -< fb
          void $ mapMaybe (arrMCl $ display Debug . ("ESPUnlock: " <>) . T.show) -< result
          void $ mapMaybe (hoistClSF withSesameConfig $ hoistClSF withUnlockConfig $ arrMCl handleUnlockEvent) -< result

withSesameConfig ::
  (Reader HomeEnv :> es) =>
  Eff (Reader SesameEnv : es) c -> Eff es c
withSesameConfig action = do
  cfg <- asks @HomeEnv (.sesame)
  case cfg of
    Nothing -> error "SesameConfig not found in environment"
    Just sesameCfg -> runReader sesameCfg action

withUnlockConfig ::
  (Reader HomeEnv :> es) =>
  Eff (Reader UnlockConfig : es) c -> Eff es c
withUnlockConfig action = do
  cfg <- asks @HomeEnv (.unlock)
  case cfg of
    Nothing -> error "UnlockConfig not found in environment"
    Just unlockCfg -> runReader unlockCfg action

mainLoop ::
  ( Reader HomeEnv :> es
  , Mqtt :> es
  , Console :> es
  , Concurrent :> es
  , IOE :> es
  ) =>
  Rhine (Eff es) (AppClock es) () ()
mainLoop =
  processMqtt @@ EffMqttClock >-- appBuffer --> processESPHeartbeat @@ ioClock waitClock

display :: (Reader HomeEnv :> es, Console :> es) => LogLevel -> T.Text -> Eff es ()
display level a = do
  minLevel <- asks @HomeEnv (.logLevel)
  if level >= minLevel
    then do
      now <- unsafeEff_ getZonedTime
      let timestamp = formatTime defaultTimeLocale "[%Y-%m-%d %H:%M:%S %Z] " now
      Console.putStrLn . TE.encodeUtf8 . (T.pack timestamp <>) $ a
    else pure ()

application ::
  ( Concurrent :> es
  , Reader HomeEnv :> es
  , Console :> es
  , IOE :> es
  , Mqtt :> es
  , Wreq :> es
  ) =>
  Eff es ()
application = do
  initializeESPresense
  flow mainLoop `concurrently_` reportMackerelMetrics

reportMackerelMetrics ::
  ( Concurrent :> es
  , Wreq :> es
  , Reader HomeEnv :> es
  , Console :> es
  ) =>
  Eff es ()
reportMackerelMetrics = do
  cfg <- asks @HomeEnv (.mackerel)
  case cfg of
    Nothing -> pure ()
    Just MackerelConfig {..} -> M.forever do
      threadDelay 1_500_000 -- 1.5 seconds
      let url = "https://api.mackerelio.com/api/v0/services/" <> T.unpack service <> "/tsdb"
          opts =
            W.defaults
              & W.header "X-Api-Key" .~ [TE.encodeUtf8 apiKey]
              & W.header "Content-Type" .~ ["application/json"]
      metrics <- atomically . flushTQueue =<< asks @HomeEnv (.mackerelMetricsQueue)
      if null metrics
        then pure ()
        else
          100 & fix \self !n -> do
            eith <- E.tryAny $ W.postWith opts url $ A.encode metrics
            case eith of
              Left exc -> do
                display Error $ "Failed to report metrics to Mackerel: " <> T.pack (show exc)
                wait <- unsafeEff_ $ randomRIO (100, n)
                display Error $ "Retrying in " <> T.pack (show wait) <> " msecs..."
                threadDelay $ wait * 1000
                self (min 1_600 $ n * 2)
              Right {} -> do
                display Info $ "Successfully reported " <> T.pack (show (length metrics)) <> " metrics to Mackerel."
                pure ()

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
          <> foldMap mqttTopicFilters config.mqtt
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
          unlock = config.unlock
          mqttDevices = config.mqtt
          logLevel = fromMaybe Info config.logLevel
      handleAny report $ withMqttClient mqttCfg \mqtt sess ->
        runEff $
          runConsole $
            runConcurrent $ do
              mackerelMetricsQueue <- newTQueueIO
              runReader HomeEnv {..} $
                runWreq $
                  runMqttWith mqtt sess application

report :: SomeException -> IO ()
report exc = do
  now <- getZonedTime
  putStrLn $ "[" ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" now ++ "] ERR: An error occurred: " ++ show exc

defaultMain :: IO ()
defaultMain = do
  CLIOpts {..} <- Opts.execParser cliOptsP
  config <- either (throwIO . userError . show) pure =<< decodeFileExact configCodec configFile
  defaultMainWith config
