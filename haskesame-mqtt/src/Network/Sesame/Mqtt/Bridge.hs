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
import Network.Mqtt.Client.AutoReconnect qualified as Mqtt
import Network.Sesame.Client qualified as Sesame
import Network.Sesame.Codec (decodeSesame5MechStatus)
import Network.Sesame.Mqtt.Types
import Network.Sesame.Types (ItemCode (MechStatus), SesamePublish (..))
import System.IO (hPutStrLn, stderr)

runBridge :: Mqtt.AutoClient -> BridgeConfig -> [BridgeDevice] -> IO ()
runBridge mqtt config devices = do
  filter_ <- either (fail . show) pure (commandFilter config)
  debug config ("subscribing command topic filter: " <> show filter_)
  _ <- Mqtt.subscribe1 mqtt filter_ Mqtt.QoS1
  debug config ("starting bridge for " <> show (Map.size deviceMap) <> " Sesame device(s)")
  race_
    (consumeCommands mqtt config deviceMap)
    (forConcurrently_ devices (publishDeviceStatus mqtt config))
  where
    deviceMap = Map.fromList [(device.deviceUuid, device.sesameClient) | device <- devices]

publishDeviceStatus :: Mqtt.AutoClient -> BridgeConfig -> BridgeDevice -> IO ()
publishDeviceStatus mqtt config device =
  forever do
    SesamePublish publishItem payload <- Sesame.readPublish device.sesameClient
    debug config ("received Sesame publish from " <> UUID.toString device.deviceUuid <> ": " <> show publishItem)
    case publishItem of
      MechStatus ->
        case decodeSesame5MechStatus payload of
          Left err -> debug config ("failed to decode mech status from " <> UUID.toString device.deviceUuid <> ": " <> show err)
          Right status -> do
            debug config ("publishing status for " <> UUID.toString device.deviceUuid)
            publishStatus mqtt config device.deviceUuid (statusFromMech status)
      _ -> pure ()

consumeCommands :: Mqtt.AutoClient -> BridgeConfig -> Map UUID Sesame.Sesame5Client -> IO ()
consumeCommands mqtt config devices =
  forever do
    message <- Mqtt.recvMessage mqtt
    case parseCommandMessage config message of
      Left err -> debug config ("ignoring MQTT command: " <> show err)
      Right (uuid, command) ->
        case Map.lookup uuid devices of
          Nothing -> debug config ("ignoring command for unknown Sesame device: " <> UUID.toString uuid)
          Just sesame -> case command of
            CommandLock -> debug config ("locking " <> UUID.toString uuid) *> Sesame.lock sesame config.historyName
            CommandUnlock -> debug config ("unlocking " <> UUID.toString uuid) *> Sesame.unlock sesame config.historyName

publishStatus :: Mqtt.AutoClient -> BridgeConfig -> UUID -> StatusPayload -> IO ()
publishStatus mqtt config uuid status = do
  topic <- either (fail . show) pure (statusTopic config uuid)
  let opts = Mqtt.PublishOptions Mqtt.QoS1 True []
  debug config ("publishing retained status to " <> show topic)
  _ <- Mqtt.publish mqtt topic (LBS.toStrict (encodeStatusPayload status)) opts
  pure ()

commandFilter :: BridgeConfig -> Either BridgeError Mqtt.TopicFilter
commandFilter config
  | "/" `T.isInfixOf` config.baseTopic = Left (InvalidBaseTopic config.baseTopic)
  | otherwise = mapTopicError (Mqtt.mkTopicFilter (config.baseTopic <> "/+/set"))

statusTopic :: BridgeConfig -> UUID -> Either BridgeError Mqtt.Topic
statusTopic config uuid
  | "/" `T.isInfixOf` config.baseTopic = Left (InvalidBaseTopic config.baseTopic)
  | otherwise = mapTopicError (Mqtt.mkTopic (config.baseTopic <> "/" <> T.pack (UUID.toString uuid) <> "/get"))

parseCommandMessage :: BridgeConfig -> Mqtt.Message -> Either BridgeError (UUID, LockCommand)
parseCommandMessage config message =
  (,)
    <$> parseCommandTopic config message.topic
    <*> parseCommandPayload message.payload

parseCommandTopic :: BridgeConfig -> Mqtt.Topic -> Either BridgeError UUID
parseCommandTopic config (Mqtt.Topic topicText) =
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

mapTopicError :: Either Mqtt.TopicError a -> Either BridgeError a
mapTopicError = either (Left . InvalidTopic . T.pack . show) Right

debug :: BridgeConfig -> String -> IO ()
debug config message =
  if config.debugLogging
    then hPutStrLn stderr ("[haskesame-mqtt] " <> message)
    else pure ()
