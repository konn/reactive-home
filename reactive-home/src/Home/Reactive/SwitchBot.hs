{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Home.Reactive.SwitchBot (
  SwitchBotConfig (..),
  SwitchBotSensor (..),
  SwitchBotSensorOptions (..),
  sensorDeviceId,
  sensorMqttTopic,
  hometricsSensorConfigs,
  switchBotConfigCodec,
  scanWindow,
  reportInterval,
  staleAfter,
  SwitchBotClock (..),
  SwitchBotUpdate (..),
  switchBotS,
  nameReadings,
  mqttRelayTopics,
  publishSensorSamples,
) where

import Control.Applicative ((<|>))
import Control.Exception.Safe (Exception, throwIO)
import Control.Monad (forM_)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Effectful (Eff, (:>))
import Effectful.Network.Mqtt (Mqtt, PublishOptions (..), PublishResult (..), QoS (..), ReasonCode, defaultPublishOptions, isSuccess, publish)
import FRP.Rhine
import GHC.Generics (Generic)
import Home.Reactive.Duration (Duration (..), seconds)
import Home.Reactive.Metrics.Hometrics
import Home.Reactive.Sensor
import Network.Mqtt.Types.Topic (Topic, mkTopic)
import Network.SwitchBot.Advertisement
import Toml ((.=))
import Toml qualified
import Validation (Validation (..))

data SwitchBotConfig = SwitchBotConfig
  { sensors :: !(Map Text SwitchBotSensor)
  , adapter :: !(Maybe Text)
  , scan_window :: !(Maybe Duration)
  , report_interval :: !(Maybe Duration)
  , stale_after :: !(Maybe Duration)
  , bluez_recovery :: !(Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)

{- | A device ID alone, or a table with per-sensor delivery options.
Preserve the input shape so Tomland can check for unknown fields exactly.
-}
data SwitchBotSensor
  = SensorId !Text
  | SensorOptions !SwitchBotSensorOptions
  deriving stock (Show, Eq, Ord, Generic)

data SwitchBotSensorOptions = SwitchBotSensorOptions
  { id :: !Text
  , mqtt_topic :: !(Maybe Text)
  , hometrics :: !HometricsSensorConfig
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Toml.HasCodec SwitchBotSensorOptions where
  hasCodec =
    Toml.table $
      SwitchBotSensorOptions
        <$> Toml.text "id" .= (.id)
        <*> Toml.dioptional (Toml.text "mqtt_topic") .= (.mqtt_topic)
        <*> hometricsSensorCodec .= (.hometrics)

sensorDeviceId :: SwitchBotSensor -> Text
sensorDeviceId (SensorId device) = device
sensorDeviceId (SensorOptions options) = options.id

sensorMqttTopic :: SwitchBotSensor -> Maybe Text
sensorMqttTopic (SensorId _) = Nothing
sensorMqttTopic (SensorOptions options) = options.mqtt_topic

hometricsSensorConfigs :: SwitchBotConfig -> Map Text HometricsSensorConfig
hometricsSensorConfigs = fmap sensorConfig . (.sensors)
  where
    sensorConfig (SensorId _) = defaultHometricsSensorConfig
    sensorConfig (SensorOptions options) = options.hometrics

instance Toml.HasCodec SwitchBotSensor where
  hasCodec key =
    Toml.dimatch matchId SensorId (Toml.text key)
      <|> Toml.dimatch matchOptions SensorOptions (Toml.hasCodec key)
    where
      matchId (SensorId device) = Just device
      matchId _ = Nothing
      matchOptions (SensorOptions options) = Just options
      matchOptions _ = Nothing

instance Toml.HasCodec SwitchBotConfig where
  hasCodec = Toml.table switchBotConfigCodec

switchBotConfigCodec :: Toml.TomlCodec SwitchBotConfig
switchBotConfigCodec =
  Toml.Codec
    { Toml.codecRead = \toml -> case Toml.codecRead base toml of
        Failure errors -> Failure errors
        Success cfg -> case validateConfig cfg of
          Left err -> Failure [Toml.BiMapError "switchbot" (Toml.ArbitraryError err)]
          Right valid -> Success valid
    , Toml.codecWrite = Toml.codecWrite base
    }
  where
    base =
      SwitchBotConfig
        <$> Toml.tableMap Toml._KeyText Toml.hasCodec "sensors" .= (.sensors)
        <*> Toml.dioptional (Toml.text "adapter") .= (.adapter)
        <*> Toml.dioptional (Toml.hasCodec "scan_window") .= (.scan_window)
        <*> Toml.dioptional (Toml.hasCodec "report_interval") .= (.report_interval)
        <*> Toml.dioptional (Toml.hasCodec "stale_after") .= (.stale_after)
        <*> Toml.dioptional (Toml.bool "bluez_recovery") .= (.bluez_recovery)

scanWindow, reportInterval, staleAfter :: SwitchBotConfig -> Duration
scanWindow = fromMaybe (seconds 5) . (.scan_window)
reportInterval = fromMaybe (seconds 60) . (.report_interval)
staleAfter = fromMaybe (seconds 120) . (.stale_after)

validateConfig :: SwitchBotConfig -> Either Text SwitchBotConfig
validateConfig cfg
  | Map.null cfg.sensors = Left "at least one SwitchBot sensor is required"
  | not (all validName $ Map.keys cfg.sensors) = Left "sensor names must use letters, digits, underscores or hyphens"
  | not (all positive [scanWindow cfg, reportInterval cfg, staleAfter cfg]) = Left "sensor durations must be finite and positive"
  | (scanWindow cfg).seconds < 0.1 || (scanWindow cfg).seconds > 60 = Left "scan_window must be between 100ms and 60s"
  | staleAfter cfg <= scanWindow cfg = Left "stale_after must be longer than scan_window"
  | reportInterval cfg >= staleAfter cfg = Left "report_interval must be shorter than stale_after"
  | otherwise = do
      normalized <- traverse normalizeSensor cfg.sensors
      if Map.size (Map.fromList [(sensorDeviceId v, ()) | v <- Map.elems normalized]) /= Map.size normalized
        then Left "each device ID may be declared only once"
        else pure ()
      let destinations =
            [ (hometricsSensorName sensor options, field)
            | (sensor, options) <- Map.toList $ hometricsSensorConfigs cfg
            , field <- hometricsFields options
            ]
      if Map.size (Map.fromList [(key, ()) | key <- destinations]) /= length destinations
        then Left "Hometrics names and fields overlap across sensors; choose different names or disjoint fields"
        else pure ()
      forM_ (hometricsSensorConfigs cfg) $ \options ->
        forM_ options.name $ \name ->
          if T.null $ T.strip name
            then Left "Hometrics sensor names must not be empty"
            else pure ()
      pure cfg {sensors = normalized}
  where
    normalizeSensor sensor = do
      device <- maybe (Left "sensor IDs must be six-byte MAC addresses") Right $ normalizeDeviceId $ sensorDeviceId sensor
      forM_ (sensorMqttTopic sensor) $ \topic -> () <$ parseRelayTopic topic
      pure $ case sensor of
        SensorId _ -> SensorId device
        SensorOptions options -> SensorOptions options {id = device}
    positive (Duration n) = n > 0 && not (isNaN n || isInfinite n)
    validName t = not (T.null t) && T.all (\c -> isAsciiLower c || isAsciiUpper c || isDigit c && c < '\x80' || c `elem` ("_-" :: String)) t

-- | An injectable scan action also permits replay without Bluetooth hardware.
newtype SwitchBotClock = SwitchBotClock (IO [SensorReading])
  deriving stock (Generic)
  deriving anyclass (GetClockProxy)

instance (MonadIO m) => Clock m SwitchBotClock where
  type Time SwitchBotClock = UTCTime
  type Tag SwitchBotClock = [SensorReading]
  initClock (SwitchBotClock scan) = do
    started <- liftIO getCurrentTime
    pure
      ( constM $ liftIO $ do
          readings <- scan
          now <- getCurrentTime
          pure (now, readings)
      , started
      )

data SwitchBotUpdate = SwitchBotUpdate
  { samples :: ![SensorSample]
  , snapshot :: !SensorSnapshot
  }
  deriving stock (Show, Eq, Ord, Generic)

nameReadings :: SwitchBotConfig -> UTCTime -> [SensorReading] -> [SensorSample]
nameReadings cfg now readings =
  [ SensorSample name now reading
  | reading <- readings
  , (name, device) <- Map.toList cfg.sensors
  , normalizeDeviceId (sensorDeviceId device) == Just reading.deviceId
  ]

-- | Name configured readings and maintain a snapshot for downstream automations.
switchBotS :: (Monad m, Time cl ~ UTCTime) => SwitchBotConfig -> ClSF m cl [SensorReading] SwitchBotUpdate
switchBotS cfg = proc readings -> do
  now <- absoluteS -< ()
  let samples = nameReadings cfg now readings
  snapshot <- sensorSnapshotS (staleAfter cfg) -< samples
  returnA -< SwitchBotUpdate samples snapshot

-- | Only sensors explicitly declaring a topic participate in MQTT delivery.
mqttRelayTopics :: SwitchBotConfig -> Map Text Text
mqttRelayTopics = Map.mapMaybe sensorMqttTopic . (.sensors)

parseRelayTopic :: Text -> Either Text Topic
parseRelayTopic = either (Left . ("invalid sensor mqtt_topic: " <>) . T.pack . show) Right . mkTopic

data SensorPublishError = SensorPublishError !ReasonCode
  deriving stock (Show)

instance Exception SensorPublishError

{- | QoS 1, non-retained snapshots. observedAt is the scan timestamp. Avoid
retained values that can remain apparently current after the sensor stops.
-}
publishSensorSamples :: (Mqtt :> es) => SwitchBotConfig -> [SensorSample] -> Eff es ()
publishSensorSamples cfg = mapM_ $ \sample ->
  forM_ (Map.lookup sample.sensor $ mqttRelayTopics cfg) $ \destination -> do
    topic <- either (throwIO . userError . T.unpack) pure $ parseRelayTopic destination
    result <- publish topic (LBS.toStrict $ encode sample) defaultPublishOptions {qos = QoS1, retain = False}
    case result of
      AckedQoS1 reason _ | not (isSuccess reason) -> throwIO $ SensorPublishError reason
      AckedQoS2 reason _ | not (isSuccess reason) -> throwIO $ SensorPublishError reason
      _ -> pure ()
