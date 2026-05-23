{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Home.Reactive.MQTT where

import Control.Concurrent.STM qualified as IO
import Data.Aeson (FromJSON)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Time (UTCTime, getCurrentTime)
import Data.Time qualified as IO
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Dispatch.Static (unsafeEff_)
import GHC.Generics (Generic)
import Network.MQTT.Client (MQTTClient, MQTTConfig (..), MessageCallback (..), Property, Topic, connectURI, mqttConfig)
import Network.URI (URI (..), URIAuth (..), escapeURIString, isReserved, isUnescapedInURIComponent)

data MQTTSettings = MQTTSettings
  { hostname :: !String
  , port :: !Int
  , username :: !(Maybe String)
  , password :: !(Maybe String)
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON)

data MQTTMessage = MQTTMessage
  { topic :: !Topic
  , payload :: !LBS.ByteString
  , properties :: ![Property]
  , timestamp :: !UTCTime
  }
  deriving (Show, Eq, Generic)

data MqttEnv = MqttEnv
  { incoming :: !(TBQueue MQTTMessage)
  , client :: !MQTTClient
  }
  deriving (Generic)

connect :: (Concurrent :> es) => MQTTSettings -> Eff es MqttEnv
connect cfg = do
  incoming <- newTBQueueIO 512
  let config =
        mqttConfig
          { _hostname = cfg.hostname
          , _port = cfg.port
          , _username = cfg.username
          , _password = cfg.password
          , _msgCB = SimpleCallback \_ topic payload properties -> do
              timestamp <- IO.getCurrentTime
              IO.atomically $ writeTBQueue incoming MQTTMessage {..}
          }
      uinfo = case cfg.password of
        Nothing -> maybe "" (escapeURIString isUnescapedInURIComponent) cfg.username
        Just pw -> maybe "" (escapeURIString isUnescapedInURIComponent) cfg.username ++ ":" ++ escapeURIString isReserved pw
  client <- unsafeEff_ do
    connectURI config $
      URI
        { uriScheme = "mqtt:"
        , uriAuthority =
            Just
              URIAuth
                { uriUserInfo = uinfo
                , uriRegName = cfg.hostname
                , uriPort = ':' : show cfg.port
                }
        , uriPath = ""
        , uriQuery = ""
        , uriFragment = ""
        }

  pure MqttEnv {..}
