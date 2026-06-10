{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
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

  -- * Re-exports
  Topic (..),
  TopicFilter (..),
  wildOne,
  wildMany,
  fromTopic,
  Message (..),
  subscribedTopics,
) where

import Control.Exception (Exception, throwIO)
import Control.Lens ((&), (.~))
import Data.Aeson (FromJSON, ToJSON)
import Data.Generics.Labels ()
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (getCurrentTime)
import Effectful (Eff, (:>))
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Network.Mqtt (Mqtt)
import Effectful.Network.Mqtt qualified as EffM
import FRP.Rhine
import GHC.Generics (Generic)
import Network.Mqtt.Client.AutoReconnect
import Toml qualified

newtype MqttClock = MqttClock AutoClient
  deriving stock (Generic)
  deriving anyclass (GetClockProxy)

data MqttDevices = MqttDevices {switches :: ![MqttSwitch]}
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

instance Toml.HasCodec MqttDevices where
  hasCodec = Toml.table Toml.genericCodec

data MqttSwitch = MqttSwitch
  { name :: {-# UNPACK #-} !T.Text
  , topic :: {-# UNPACK #-} !Topic
  , onValue :: !(Maybe T.Text)
  , offValue :: !(Maybe T.Text)
  }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

subscribedTopics :: MqttDevices -> [Topic]
subscribedTopics MqttDevices {..} = map (.topic) switches

instance Toml.HasCodec MqttSwitch where
  hasCodec = Toml.table Toml.genericCodec

instance Toml.HasItemCodec MqttSwitch where
  hasItemCodec = Right Toml.genericCodec

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
  , subscriptions :: !(NonEmpty Subscription)
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
      reasons <- subscribe client config.subscriptions []
      let !failures =
            NE.nonEmpty $
              NE.filter (\(_, reason) -> not $ isSuccess reason) $
                NE.zip config.subscriptions reasons
      case failures of
        Just errors -> throwIO $ SubscriptionFailed errors
        Nothing -> k client session
