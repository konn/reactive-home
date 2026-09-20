{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Home.Reactive.MQTT (
  withMqttClient,
  MqttMessage,
  MqttClient,
  MqttSession,
  MqttClockConfig (..),
  newMqttClock,
  MqttClock (..),
  EffMqttClock (..),
  MqttClockError (..),

  -- * subscriptions
  mqttTopicFilters,
  MqttDevices (..),
  MqttScheduledSwitch (..),

  -- * Re-exports
  Topic (..),
  TopicFilter (..),
  wildOne,
  wildMany,
  fromTopic,
  Message (..),
  switchStateS,
  mqttSnapshotS,
  MqttSnapshot (..),
) where

import Control.Exception (Exception, throwIO)
import Control.Lens ((&), (.~))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as A
import Data.Generics.Labels ()
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (getCurrentTime)
import Effectful (Eff, (:>))
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Network.Mqtt (Mqtt)
import Effectful.Network.Mqtt qualified as EffM
import Effectful.Reader.Static (Reader)
import FRP.Rhine
import GHC.Generics (Generic)
import Home.Reactive.Duration (Duration (..))
import Home.Reactive.Utils (catMaybesS, effReaderS)
import Network.Mqtt.Client.AutoReconnect hiding (Success)
import Toml qualified
import Validation (Validation (..))

newtype MqttClock = MqttClock AutoClient
  deriving stock (Generic)
  deriving anyclass (GetClockProxy)

data MqttDevices = MqttDevices
  { switches :: ![MqttSwitch]
  , scheduled_switches :: ![MqttScheduledSwitch]
  }
  deriving (Show, Eq, Ord, Generic, ToJSON)
  deriving (Toml.HasCodec, Toml.HasItemCodec) via Toml.TomlTable MqttDevices

instance FromJSON MqttDevices where
  parseJSON = A.withObject "MqttDevices" \obj ->
    MqttDevices
      <$> obj A..:? "switches" A..!= []
      <*> obj A..:? "scheduled_switches" A..!= []

data MqttSwitch = MqttSwitch
  { name :: {-# UNPACK #-} !T.Text
  , topic :: {-# UNPACK #-} !Topic
  , onValue :: !(Maybe T.Text)
  , offValue :: !(Maybe T.Text)
  }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)
  deriving (Toml.HasCodec, Toml.HasItemCodec) via Toml.TomlTable MqttSwitch

data MqttScheduledSwitch = MqttScheduledSwitch
  { name :: {-# UNPACK #-} !T.Text
  , topic :: {-# UNPACK #-} !Topic
  , interval :: !Duration
  , on_duration :: !Duration
  }
  deriving (Show, Eq, Ord, Generic, ToJSON)

instance FromJSON MqttScheduledSwitch where
  parseJSON value = do
    switch <- A.genericParseJSON A.defaultOptions value
    either (fail . T.unpack . snd) pure (validateScheduledSwitch switch)

instance Toml.HasCodec MqttScheduledSwitch where
  hasCodec = Toml.table scheduledSwitchCodec

instance Toml.HasItemCodec MqttScheduledSwitch where
  hasItemCodec = Right scheduledSwitchCodec

scheduledSwitchCodec :: Toml.TomlCodec MqttScheduledSwitch
scheduledSwitchCodec =
  Toml.Codec
    { Toml.codecRead = \toml ->
        case Toml.codecRead baseCodec toml of
          Failure errors -> Failure errors
          Success switch ->
            case validateScheduledSwitch switch of
              Left (key, message) -> Failure [Toml.BiMapError key (Toml.ArbitraryError message)]
              Right valid -> Success valid
    , Toml.codecWrite = Toml.codecWrite baseCodec
    }
  where
    baseCodec = Toml.genericCodec @MqttScheduledSwitch

validateScheduledSwitch :: MqttScheduledSwitch -> Either (Toml.Key, T.Text) MqttScheduledSwitch
validateScheduledSwitch switch
  | not (positiveFinite switch.interval) = Left ("interval", "interval must be a finite, positive duration")
  | not (positiveFinite switch.on_duration) = Left ("on_duration", "on_duration must be a finite, positive duration")
  | switch.on_duration >= switch.interval = Left ("on_duration", "on_duration must be shorter than interval")
  | otherwise = Right switch
  where
    positiveFinite (Duration secs) = secs > 0 && not (isNaN secs || isInfinite secs)

data MqttSnapshot = MqttSnapshot {switches :: HashMap T.Text Bool}
  deriving (Show, Eq, Ord, Generic)

mqttSnapshotS ::
  (Reader MqttDevices :> es) =>
  ClSF (Eff es) cl Message MqttSnapshot
mqttSnapshotS = effReaderS @MqttDevices proc (msg, devices) -> do
  switches <-
    parallely
      (proc (msg, sw) -> switchStateS -< (sw, msg))
      -<
        HM.fromList [(sw.name, (msg, sw)) | sw <- observedSwitches devices]
  returnA -< MqttSnapshot {..}

switchStateS ::
  ClSF (Eff es) cl (MqttSwitch, Message) Bool
switchStateS =
  catMaybesS False <-< proc (switch, msg) -> do
    let payload = TE.decodeUtf8 msg.payload
        onValue = fromMaybe "true" switch.onValue
        offValue = fromMaybe "false" switch.offValue
    returnA
      -<
        if msg.topic == switch.topic
          then
            if
              | payload == onValue -> Just True
              | payload == offValue -> Just False
              | otherwise -> Nothing
          else Nothing

mqttTopicFilters :: MqttDevices -> [TopicFilter]
mqttTopicFilters = map (fromTopic . (.topic)) . observedSwitches

observedSwitches :: MqttDevices -> [MqttSwitch]
observedSwitches devices =
  devices.switches
    <> [ MqttSwitch {name = sw.name, topic = sw.topic, onValue = Nothing, offValue = Nothing}
       | sw <- devices.scheduled_switches
       ]

newMqttClock :: MqttClient -> MqttClock
{-# INLINE newMqttClock #-}
newMqttClock = MqttClock

type MqttMessage = Message

instance {-# OVERLAPPABLE #-} (MonadIO m) => Clock m MqttClock where
  type Time MqttClock = UTCTime
  type Tag MqttClock = Message
  initClock (MqttClock client) = do
    initialTime <- liftIO getCurrentTime
    let runningClock = constM $ liftIO do
          msg <- recvMessage client
          time <- getCurrentTime
          pure (time, msg)
    pure (runningClock, initialTime)

data EffMqttClock = EffMqttClock
  deriving stock (Generic)
  deriving anyclass (GetClockProxy)

instance {-# OVERLAPS #-} (Mqtt :> es) => Clock (Eff es) EffMqttClock where
  type Time EffMqttClock = UTCTime
  type Tag EffMqttClock = Message
  initClock EffMqttClock = do
    initialTime <- unsafeEff_ getCurrentTime
    let runningClock = constM do
          msg <- EffM.recvMessage
          time <- unsafeEff_ getCurrentTime
          pure (time, msg)
    pure (runningClock, initialTime)

data MqttClockConfig = MqttClockConfig
  { host :: !String
  , port :: !Int
  , user :: !(Maybe T.Text)
  , password :: !(Maybe T.Text)
  , clientId :: !T.Text
  , subscriptions :: ![Subscription]
  }
  deriving (Show, Eq, Generic)

type MqttClient = AutoClient

type MqttSession = Session

newtype MqttClockError = SubscriptionFailed (NonEmpty (Subscription, ReasonCode))
  deriving (Show, Eq, Generic)
  deriving anyclass (Exception)

withMqttClient :: MqttClockConfig -> (MqttClient -> MqttSession -> IO a) -> IO a
withMqttClient config k = do
  let factory =
        tcpConnection $
          clientSettings config.host $
            fromIntegral config.port
  withClient
    ( (defaultConnectOptions factory config.clientId)
        & #username .~ config.user
        & #password .~ (TE.encodeUtf8 <$> config.password)
    )
    defaultAutoReconnectConfig
    \client session -> do
      failures <- case NE.nonEmpty config.subscriptions of
        Nothing -> pure Nothing
        Just requestedSubscriptions -> do
          reasons <- subscribe client requestedSubscriptions []
          pure $ NE.nonEmpty $ NE.filter (\(_, reason) -> not $ isSuccess reason) $ NE.zip requestedSubscriptions reasons
      case failures of
        Just errors -> throwIO $ SubscriptionFailed errors
        Nothing -> k client session
