module Network.Sesame.Mqtt.Bridge (
  runBridge,
  publishStatus,
  commandFilter,
  statusTopic,
  parseCommandTopic,
  parseCommandPayload,
  parseCommandMessage,
) where

import Control.Concurrent.Async (forConcurrently_, race_)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Network.Mqtt.Client qualified as Mqtt
import Network.Mqtt.Message (Message (..))
import Network.Mqtt.Types
import Network.Sesame.Client qualified as Sesame
import Network.Sesame.Codec (decodeSesame5MechStatus)
import Network.Sesame.Mqtt.Types
import Network.Sesame.Types (ItemCode (MechStatus), SesamePublish (..))

runBridge :: Mqtt.Client -> BridgeConfig -> [BridgeDevice] -> IO ()
runBridge mqtt config devices = do
  filter_ <- either (fail . show) pure (commandFilter config)
  _ <- Mqtt.subscribe1 mqtt filter_ QoS1
  race_
    (consumeCommands mqtt config deviceMap)
    (forConcurrently_ devices (publishDeviceStatus mqtt config))
  where
    deviceMap = Map.fromList [(device.deviceUuid, device.sesameClient) | device <- devices]

publishDeviceStatus :: Mqtt.Client -> BridgeConfig -> BridgeDevice -> IO ()
publishDeviceStatus mqtt config device =
  forever do
    SesamePublish publishItem payload <- Sesame.readPublish device.sesameClient
    case publishItem of
      MechStatus ->
        case decodeSesame5MechStatus payload of
          Left _ -> pure ()
          Right status -> publishStatus mqtt config device.deviceUuid (statusFromMech status)
      _ -> pure ()

consumeCommands :: Mqtt.Client -> BridgeConfig -> Map UUID Sesame.Sesame5Client -> IO ()
consumeCommands mqtt config devices =
  forever do
    message <- Mqtt.recvMessage mqtt
    case parseCommandMessage config message of
      Left _ -> pure ()
      Right (uuid, command) ->
        case Map.lookup uuid devices of
          Nothing -> pure ()
          Just sesame -> case command of
            CommandLock -> Sesame.lock sesame config.historyName
            CommandUnlock -> Sesame.unlock sesame config.historyName

publishStatus :: Mqtt.Client -> BridgeConfig -> UUID -> StatusPayload -> IO ()
publishStatus mqtt config uuid status = do
  topic <- either (fail . show) pure (statusTopic config uuid)
  let opts = Mqtt.PublishOptions QoS1 True []
  _ <- Mqtt.publish mqtt topic (LBS.toStrict (encodeStatusPayload status)) opts
  pure ()

commandFilter :: BridgeConfig -> Either BridgeError TopicFilter
commandFilter config
  | "/" `T.isInfixOf` config.baseTopic = Left (InvalidBaseTopic config.baseTopic)
  | otherwise = mapTopicError (mkTopicFilter (config.baseTopic <> "/+/set"))

statusTopic :: BridgeConfig -> UUID -> Either BridgeError Topic
statusTopic config uuid
  | "/" `T.isInfixOf` config.baseTopic = Left (InvalidBaseTopic config.baseTopic)
  | otherwise = mapTopicError (mkTopic (config.baseTopic <> "/" <> T.pack (UUID.toString uuid) <> "/get"))

parseCommandMessage :: BridgeConfig -> Message -> Either BridgeError (UUID, LockCommand)
parseCommandMessage config message =
  (,)
    <$> parseCommandTopic config message.topic
    <*> parseCommandPayload message.payload

parseCommandTopic :: BridgeConfig -> Topic -> Either BridgeError UUID
parseCommandTopic config (Topic topicText) =
  case T.splitOn "/" topicText of
    [base, uuidText, "set"]
      | base == config.baseTopic ->
          maybe (Left (InvalidTopic topicText)) Right (UUID.fromString (T.unpack uuidText))
    _ -> Left (InvalidTopic topicText)

parseCommandPayload :: ByteString -> Either BridgeError LockCommand
parseCommandPayload payload =
  case TE.decodeUtf8' payload of
    Left _ -> Left (InvalidCommandPayload "<non-utf8>")
    Right "LOCKED" -> Right CommandLock
    Right "UNLOCKED" -> Right CommandUnlock
    Right other -> Left (InvalidCommandPayload other)

mapTopicError :: Either TopicError a -> Either BridgeError a
mapTopicError = either (Left . InvalidTopic . T.pack . show) Right
