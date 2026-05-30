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
import Control.Exception (displayException, throwIO)
import Control.Exception.Safe (tryAny)
import Control.Lens ((&), (.~))
import Control.Monad.Trans.Class (lift)
import Data.Aeson (ToJSON)
import Data.Aeson qualified as A
import Data.ByteString.Char8 qualified as BS8
import Data.Functor (void, (<&>))
import Data.Generics.Labels ()
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Effectful
import Effectful.Concurrent (Concurrent, forkIO, runConcurrent)
import Effectful.Console.ByteString (Console, runConsole)
import Effectful.Console.ByteString qualified as Console
import Effectful.Console.ByteString qualified as Eff
import Effectful.Network.Mqtt
import Effectful.Reader.Static (Reader, asks, runReader)
import Effectful.Wreq (Wreq, postWith, runWreq)
import Effectful.Wreq qualified as W
import FRP.Rhine
import GHC.Generics (Generic)
import Home.Reactive.App.Types (ParseResult (..))
import Home.Reactive.ESPresense
import Home.Reactive.MQTT
import Home.Reactive.Sesame5
import Options.Applicative qualified as Opts
import Toml hiding (first, map)

data Config = Config
  { host :: !T.Text
  , port :: !Int
  , user :: !(Maybe T.Text)
  , password :: !(Maybe T.Text)
  , espresense :: !(Maybe ESPresenseConfig)
  , sesame :: !(Maybe SesameConfig)
  , mackerel :: !(Maybe MackerelConfig)
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (HasCodec) via TomlTable Config

data MackerelConfig = MackerelConfig
  { service :: !T.Text
  , apiKey :: !T.Text
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (HasItemCodec, HasCodec) via TomlTable MackerelConfig

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
  , Wreq :> es
  , Concurrent :> es
  ) =>
  ClSF (Eff es) EffMqttClock Message ()
processSesame = proc msg -> do
  msess <- constMCl (asks @HomeEnv (.sesame)) -< ()
  parsed <- sesameStatuses -< (msess, msg)
  reported <- reportErrors -< parsed
  void (mapMaybeS postMackerelS &&& aggregateSesameStatus) -< reported

processESP ::
  ( Console :> es
  , Reader HomeEnv :> es
  , Wreq :> es
  , Concurrent :> es
  ) =>
  ClSF (Eff es) EffMqttClock Message ()
processESP =
  parseESPStatusS
    >-> reportErrors
    >-> void (mapMaybeS postMackerelS &&& aggregateESPStatus)

postMackerelS ::
  ( Wreq :> es
  , Reader HomeEnv :> es
  , Concurrent :> es
  , ToMackerelMetrics a
  , Console :> es
  ) =>
  ClSF (Eff es) cl a ()
postMackerelS = proc stt -> do
  mackerel <- constMCl (asks @HomeEnv (.mackerel)) -< ()
  case mackerel of
    Just cfg -> arrMCl (uncurry postMackerel) -< (cfg, stt)
    Nothing -> returnA -< ()

data MackerelMetrics = MackerelEntry
  { name :: !T.Text
  , time :: !UTCTime
  , value :: !A.Value
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON MackerelMetrics where
  toJSON (MackerelEntry {..}) =
    A.object
      [ "name" A..= name
      , "time" A..= floor @_ @Int (utcTimeToPOSIXSeconds time)
      , "value" A..= value
      ]

class ToMackerelMetrics a where
  toMetrics :: a -> [MackerelMetrics]

instance ToMackerelMetrics ESPStatus where
  toMetrics stt =
    [ MackerelEntry
        { name = "espresense.distance." <> stt.sensor
        , time = stt.timestamp
        , value = A.Number (realToFrac stt.distance)
        }
    , MackerelEntry
        { name = "espresense.variance." <> stt.sensor
        , time = stt.timestamp
        , value = A.Number (realToFrac stt.var)
        }
    ]

instance ToMackerelMetrics SesameStatus where
  toMetrics stt =
    [ MackerelEntry
        { name = "sesame.position." <> stt.name
        , time = stt.lastUpdated
        , value = A.toJSON stt.position
        }
    , MackerelEntry
        { name = "sesame.lockCurrentState." <> stt.name
        , time = stt.lastUpdated
        , value = case stt.lockCurrentState of
            LOCKED -> A.Number 1
            UNLOCKED -> A.Number 0
        }
    , MackerelEntry
        { name = "sesame.batteryVoltage." <> stt.name
        , time = stt.lastUpdated
        , value = A.toJSON stt.batteryVoltage
        }
    , MackerelEntry
        { name = "sesame.batteryLevel." <> stt.name
        , time = stt.lastUpdated
        , value = A.toJSON stt.batteryLevel
        }
    , MackerelEntry
        { name = "sesame.statusLowBattery." <> stt.name
        , time = stt.lastUpdated
        , value = if stt.statusLowBattery then A.Number 1 else A.Number 0
        }
    ]

postMackerel ::
  ( Concurrent :> es
  , Wreq :> es
  , ToMackerelMetrics s
  , Console :> es
  ) =>
  MackerelConfig -> s -> Eff es ()
postMackerel MackerelConfig {..} s = do
  let url = "https://api.mackerelio.com/api/v0/services/" <> T.unpack service <> "/tsdb"
      opts =
        W.defaults
          & W.header "X-Api-Key" .~ [TE.encodeUtf8 apiKey]
          & W.header "Content-Type" .~ ["application/json"]
  void $ forkIO $ tryAnyReport $ postWith opts url $ A.encode $ toMetrics s

tryAnyReport :: (Console :> es) => Eff es a -> Eff es ()
tryAnyReport act = do
  tryAny act >>= \case
    Left err -> Eff.putStrLn $ TE.encodeUtf8 $ T.pack $ "Failed to post to Mackerel: " <> displayException err
    Right _ -> pure ()

mainLogic ::
  (Reader HomeEnv :> es, Console :> es, Wreq :> es, Concurrent :> es) =>
  ClSF (Eff es) EffMqttClock () ()
mainLogic = void (arrMCl (Console.putStrLn . BS8.pack . show) &&& processSesame &&& processESP) <-< tagS

defaultMainWith :: Config -> IO ()
defaultMainWith config = do
  print config
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
              , clientId = "reactive-home-client"
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
      withMqttClient mqttCfg \mqtt sess ->
        runEff $
          runConsole $
            runWreq $
              runReader HomeEnv {..} $
                runMqttWith mqtt sess $
                  runConcurrent do
                    flow $ mainLogic @@ EffMqttClock

defaultMain :: IO ()
defaultMain = do
  CLIOpts {..} <- Opts.execParser cliOptsP
  config <- either (throwIO . userError . show) pure =<< decodeFileExact configCodec configFile
  defaultMainWith config
