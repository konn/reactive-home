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

module Main (main) where

import Control.Applicative ((<**>))
import Control.Exception (Exception, displayException, throwIO)
import Control.Exception.Safe (tryAny)
import Control.Lens (view, (&), (.~))
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable qualified as F
import Data.Functor (void, (<&>))
import Data.Generics.Labels ()
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.List.NonEmpty qualified as NE
import Data.String (IsString)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Effectful
import Effectful.Concurrent (Concurrent, forkIO, runConcurrent)
import Effectful.Console.ByteString (Console, runConsole)
import Effectful.Console.ByteString qualified as Eff
import Effectful.Network.Mqtt
import Effectful.Reader.Static (Reader, asks, runReader)
import Effectful.Wreq (Wreq, postWith, runWreq)
import Effectful.Wreq qualified as W
import FRP.Rhine
import GHC.Generics (Generic)
import Home.Reactive.MQTT
import Options.Applicative qualified as Opts
import Toml hiding (first, map)

class ToTopicFilter a where
  toTopicFilters :: a -> [TopicFilter]

data ESPresenseConfig = ESPresenseConfig
  { devices :: ![T.Text]
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (HasCodec, HasItemCodec) via TomlTable ESPresenseConfig

instance ToTopicFilter ESPresenseConfig where
  toTopicFilters ESPresenseConfig {..} =
    [ "espresense" <> "devices" <> TopicFilter device <> wildOne
    | device <- devices
    ]

data SesameDevice = SesameDevice {uuid :: SesameUUID}
  deriving (Show, Eq, Ord, Generic)
  deriving (HasCodec, HasItemCodec) via TomlTable SesameDevice

data SesameConfig = SesameConfig
  { prefix :: !T.Text
  , devices :: !(HashMap T.Text SesameDevice)
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (HasCodec, HasItemCodec) via TomlTable SesameConfig

fromSesameConfig :: SesameConfig -> SesameEnv
fromSesameConfig SesameConfig {..} =
  SesameEnv
    { prefix = prefix
    , devices = devices
    , uuids = HM.fromList $ map (\(name, dev) -> (dev.uuid, (name, dev))) $ HM.toList devices
    }

newtype SesameUUID = UUID {raw :: T.Text}
  deriving newtype
    ( IsString
    , Show
    , Eq
    , Ord
    , HasItemCodec
    , HasCodec
    , Hashable
    )

data SesameEnv = SesameEnv
  { prefix :: !T.Text
  , devices :: !(HashMap T.Text SesameDevice)
  , uuids :: !(HashMap SesameUUID (T.Text, SesameDevice))
  }
  deriving (Show, Eq, Ord, Generic)

instance ToTopicFilter SesameConfig where
  toTopicFilters :: SesameConfig -> [TopicFilter]
  toTopicFilters SesameConfig {..}
    | null devices =
        [ TopicFilter prefix <> wildOne <> action
        | action <- ["set", "get"]
        ]
    | otherwise =
        [ TopicFilter prefix <> TopicFilter device.uuid.raw <> action
        | device <- F.toList devices
        , action <- ["set", "get"]
        ]

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

data LockStatus = LOCKED | UNLOCKED
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data RawSesameStatus = SesamePayload
  { position :: {-# UNPACK #-} !Int
  , lockCurrentState :: {-# UNPACK #-} !LockStatus
  , batteryVoltage :: {-# UNPACK #-} !Double
  , batteryLevel :: {-# UNPACK #-} !Int
  , chargingState :: {-# UNPACK #-} !T.Text
  , statusLowBattery :: {-# UNPACK #-} !Bool
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SesameStatus = SesameStatus
  { name :: {-# UNPACK #-} !T.Text
  , uuid :: {-# UNPACK #-} !SesameUUID
  , lastUpdated :: {-# UNPACK #-} !UTCTime
  , position :: {-# UNPACK #-} !Int
  , lockCurrentState :: {-# UNPACK #-} !LockStatus
  , batteryVoltage :: {-# UNPACK #-} !Double
  , batteryLevel :: {-# UNPACK #-} !Int
  , statusLowBattery :: {-# UNPACK #-} !Bool
  }
  deriving (Show, Eq, Ord, Generic)

data ParseResult e a
  = ParseSuccess a
  | ParseFailure e
  | Skipped
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

data Action
  = LockSesame !T.Text
  | UnlockSesame !T.Text
  deriving (Show, Eq, Ord, Generic)

data HomeEnv = HomeEnv
  { mqtt :: {-# UNPACK #-} !MqttClient
  , sesame :: {-# UNPACK #-} !(Maybe SesameEnv)
  , espresense :: !(Maybe ESPresenseConfig)
  , mackerel :: !(Maybe MackerelConfig)
  }
  deriving (Generic)

data SesameError
  = InvalidMessage !MqttMessage
  | UnknownDevice !T.Text
  | InvalidPayload !String
  deriving (Show, Eq, Generic)
  deriving anyclass (Exception)

parseSesameStatus ::
  SesameEnv ->
  MqttMessage ->
  ParseResult SesameError (T.Text, SesameDevice, RawSesameStatus)
parseSesameStatus env msg =
  case stripPrefix env.prefix msg.topic of
    Nothing -> Skipped
    Just (Topic rest)
      | (uuid, slashAction) <- T.breakOn "/" rest
      , let action = T.drop 1 slashAction
      , not (T.null action) ->
          case action of
            "get"
              | Just (name, device) <- HM.lookup (UUID uuid) env.uuids ->
                  case A.eitherDecode' $ LBS.fromStrict msg.payload of
                    Left err -> ParseFailure $ InvalidPayload err
                    Right stt -> ParseSuccess (name, device, stt)
              | otherwise -> ParseFailure (UnknownDevice uuid)
            _ -> Skipped
      | otherwise -> Skipped

sesameStatuses ::
  (Reader HomeEnv :> es) =>
  BehaviourF
    (Eff es)
    UTCTime
    MqttMessage
    (ParseResult SesameError SesameStatus)
sesameStatuses = proc msg -> do
  msess <- constMCl (asks @HomeEnv $ view #sesame) -< ()
  TimeInfo {..} <- timeInfo -< ()
  returnA
    -< case msess of
      Nothing -> Skipped
      Just sess ->
        parseSesameStatus sess msg
          <&> \(name, device, stat) ->
            buildSesameStatus absolute name device stat

buildSesameStatus ::
  UTCTime ->
  T.Text ->
  SesameDevice ->
  RawSesameStatus ->
  SesameStatus
buildSesameStatus absolute name device stat =
  SesameStatus
    { name
    , uuid = device.uuid
    , lastUpdated = absolute
    , position = stat.position
    , lockCurrentState = stat.lockCurrentState
    , batteryVoltage = stat.batteryVoltage
    , batteryLevel = stat.batteryLevel
    , statusLowBattery = stat.statusLowBattery
    }

reportErrors ::
  (Show e, Console :> es) =>
  ClSF (Eff es) cl (ParseResult e a) (Maybe a)
reportErrors = arrM \case
  ParseFailure err -> do
    lift $ Eff.putStrLn $ TE.encodeUtf8 $ T.pack $ "Error: " <> show err
    pure Nothing
  ParseSuccess a -> pure (Just a)
  Skipped -> pure Nothing

aggregateSesameStatus ::
  BehaviourF (Eff es) UTCTime (Maybe SesameStatus) (HashMap T.Text SesameStatus)
aggregateSesameStatus =
  feedback HM.empty $ arr \(mmsg, prev) ->
    let new = case mmsg of
          Nothing -> prev
          Just stt -> HM.insert stt.name stt prev
     in (new, new)

processSesame ::
  ( Reader HomeEnv :> es
  , Console :> es
  , Wreq :> es
  , Concurrent :> es
  ) =>
  ClSF (Eff es) EffMqttClock Message ()
processSesame =
  sesameStatuses
    >-> reportErrors
    >-> void
      (mapMaybeS postMackerelS &&& aggregateSesameStatus)

type ESPSensorName = T.Text

type ESPDeviceId = T.Text

data RawESPStatus = RawESPStatus
  { mac :: {-# UNPACK #-} !T.Text
  , id :: {-# UNPACK #-} !T.Text
  , name :: {-# UNPACK #-} !T.Text
  , rssi :: {-# UNPACK #-} !Float
  , rssiVar :: {-# UNPACK #-} !Float
  , distance :: {-# UNPACK #-} !Float
  , var :: {-# UNPACK #-} !Float
  , int :: {-# UNPACK #-} !Int
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ESPStatus = ESPStatus
  { timestamp :: {-# UNPACK #-} !UTCTime
  , sensor :: {-# UNPACK #-} !T.Text
  , mac :: {-# UNPACK #-} !T.Text
  , id :: {-# UNPACK #-} !T.Text
  , name :: {-# UNPACK #-} !T.Text
  , rssi :: {-# UNPACK #-} !Float
  , rssiVar :: {-# UNPACK #-} !Float
  , distance :: {-# UNPACK #-} !Float
  , var :: {-# UNPACK #-} !Float
  , int :: {-# UNPACK #-} !Int
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ESPSensorState = ESPSensorState
  { timestamp :: {-# UNPACK #-} !UTCTime
  , distance :: {-# UNPACK #-} !Float
  , variance :: {-# UNPACK #-} !Float
  , interval :: {-# UNPACK #-} !Int
  }
  deriving (Show, Eq, Ord, Generic)

parseRawESPStatus ::
  MqttMessage ->
  ParseResult String (T.Text, RawESPStatus)
parseRawESPStatus msg =
  case stripPrefix "espresense/devices" msg.topic of
    Nothing -> Skipped
    Just (Topic rest) ->
      case T.splitOn "/" rest of
        [_, sensor] ->
          case A.eitherDecode' $ LBS.fromStrict msg.payload of
            Left err -> ParseFailure err
            Right stt -> ParseSuccess (sensor, stt)
        _ -> Skipped

buildESPStatus :: UTCTime -> T.Text -> RawESPStatus -> ESPStatus
buildESPStatus timestamp sensor stt =
  ESPStatus
    { timestamp
    , sensor
    , mac = stt.mac
    , id = stt.id
    , name = stt.name
    , rssi = stt.rssi
    , rssiVar = stt.rssiVar
    , distance = stt.distance
    , var = stt.var
    , int = stt.int
    }

parseESPStatusS ::
  (Time cl ~ UTCTime) =>
  ClSF (Eff es) cl MqttMessage (ParseResult String ESPStatus)
parseESPStatusS = proc msg -> do
  TimeInfo {..} <- timeInfo -< ()
  returnA -< uncurry (buildESPStatus absolute) <$> parseRawESPStatus msg

postMackerelS ::
  ( Wreq :> es
  , Reader HomeEnv :> es
  , Concurrent :> es
  , ToMackerelMetrics a
  , Console :> es
  ) =>
  ClSF (Eff es) cl a ()
postMackerelS = proc stt -> do
  mackerel <- constMCl (asks @HomeEnv $ view #mackerel) -< ()
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

aggregateESPStatus ::
  BehaviourF (Eff es) UTCTime (Maybe ESPStatus) (HashMap (ESPSensorName, ESPDeviceId) ESPSensorState)
aggregateESPStatus = feedback HM.empty $ arr \(!mest, !prev) ->
  case mest of
    Nothing -> (prev, prev)
    Just stt ->
      let ssst =
            ESPSensorState
              { timestamp = stt.timestamp
              , distance = stt.distance
              , variance = stt.var
              , interval = stt.int
              }
          !new = HM.insert (stt.sensor, stt.id) ssst prev
       in (new, new)

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

mainLogic ::
  (Reader HomeEnv :> es, Console :> es, Wreq :> es, Concurrent :> es) =>
  ClSF (Eff es) EffMqttClock () ()
mainLogic = void (processSesame &&& processESP) <-< tagS

main :: IO ()
main = do
  CLIOpts {..} <- Opts.execParser cliOptsP
  config <- either (throwIO . userError . show) pure =<< decodeFileExact configCodec configFile
  print config
  let !topics =
        foldMap toTopicFilters config.espresense
          <> foldMap toTopicFilters config.sesame
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
