module Network.Sesame.Mqtt.Bluez.App (
  AppConfig (..),
  MqttConfig (..),
  BridgeTomlConfig (..),
  DeviceConfig (..),
  configCodec,
  loadConfig,
  runApp,
) where

import Control.Exception qualified as Exception
import DBus (objectPath_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (digitToInt, isHexDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID qualified as UUID
import GHC.Generics (Generic)
import Network.Mqtt.Client.AutoReconnect qualified as Mqtt
import Network.Sesame.Client qualified as Sesame
import Network.Sesame.Mqtt (BridgeConfig (..), BridgeDevice (..), ConnectedBridgeDevice (..), runBridge)
import Network.Sesame.Transport (SesameTransport (..))
import Network.Sesame.Transport.Bluez (BluezConfig (..), connectBluez)
import Network.Sesame.Types (Advertisement (..), SecretKey (..))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)
import Toml hiding (map)

data AppConfig = AppConfig
  { mqtt :: !MqttConfig
  , bridge :: !BridgeTomlConfig
  , devices :: ![DeviceConfig]
  }
  deriving stock (Show, Eq, Generic)
  deriving (HasCodec) via TomlTable AppConfig

data MqttConfig = MqttConfig
  { host :: !Text
  , port :: !Int
  , user :: !(Maybe Text)
  , password :: !(Maybe Text)
  , client_id :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving (HasCodec, HasItemCodec) via TomlTable MqttConfig

data BridgeTomlConfig = BridgeTomlConfig
  { base_topic :: !Text
  , history_name :: !Text
  , debug_logging :: !(Maybe Bool)
  }
  deriving stock (Show, Eq, Generic)
  deriving (HasCodec, HasItemCodec) via TomlTable BridgeTomlConfig

data DeviceConfig = DeviceConfig
  { uuid :: !(Maybe Text)
  , mac_address :: !(Maybe Text)
  , secret_key :: !Text
  , device_path :: !(Maybe Text)
  , write_characteristic_path :: !(Maybe Text)
  , notify_characteristic_path :: !(Maybe Text)
  , manufacturer_data :: !(Maybe Text)
  , command_timeout_ms :: !(Maybe Int)
  }
  deriving stock (Show, Eq, Generic)
  deriving (HasCodec, HasItemCodec) via TomlTable DeviceConfig

configCodec :: TomlCodec AppConfig
configCodec = genericCodec

runApp :: AppConfig -> IO ()
runApp config = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  Mqtt.withClient (mqttOptions config.mqtt) Mqtt.defaultAutoReconnectConfig \mqtt _ ->
    prepareDevices (bridgeDebugLogging config.bridge) config.devices >>= runBridge mqtt (bridgeConfig config.bridge)

loadConfig :: FilePath -> IO AppConfig
loadConfig configFile =
  either (fail . show) pure =<< decodeFileExact configCodec configFile

mqttOptions :: MqttConfig -> Mqtt.ConnectOptions
mqttOptions config =
  case Mqtt.defaultConnectOptions connection clientId of
    Mqtt.ConnectOptions factory cid clean keepAlive _ _ will properties authenticator topicAliases queueBound overflow ->
      Mqtt.ConnectOptions factory cid clean keepAlive config.user (TE.encodeUtf8 <$> config.password) will properties authenticator topicAliases queueBound overflow
  where
    connection = Mqtt.tcpConnection (Mqtt.clientSettings (T.unpack config.host) (fromIntegral config.port))
    clientId = maybe "haskesame-mqtt-bluez" id config.client_id

bridgeConfig :: BridgeTomlConfig -> BridgeConfig
bridgeConfig config =
  BridgeConfig
    { baseTopic = config.base_topic
    , historyName = config.history_name
    , debugLogging = bridgeDebugLogging config
    }

bridgeDebugLogging :: BridgeTomlConfig -> Bool
bridgeDebugLogging config = maybe False id config.debug_logging

prepareDevices :: Bool -> [DeviceConfig] -> IO [BridgeDevice]
prepareDevices debugLogging = traverse (prepareDevice debugLogging)

prepareDevice :: Bool -> DeviceConfig -> IO BridgeDevice
prepareDevice debugLogging config = do
  uuid <- case config.uuid of
    Just uuidText -> maybe (fail ("invalid Sesame UUID: " <> T.unpack uuidText)) pure (UUID.fromString (T.unpack uuidText))
    Nothing -> do
      connected <- connectDevice debugLogging config
      discoveredUuid <- deviceUuid config connected.sesameTransport
      connected.sesameTransport.closeBle
      pure discoveredUuid
  pure
    BridgeDevice
      { deviceUuid = uuid
      , connectSesameClient = do
          next <- connectDevice debugLogging config
          pure
            ConnectedBridgeDevice
              { sesameClient = next.sesameClient
              , disconnectSesameClient = next.sesameTransport.closeBle
              }
      }

data ConnectedSesameDevice = ConnectedSesameDevice
  { sesameClient :: !Sesame.Sesame5Client
  , sesameTransport :: !SesameTransport
  }

connectDevice :: Bool -> DeviceConfig -> IO ConnectedSesameDevice
connectDevice debugLogging config = do
  secret <- either fail pure (decodeHexText "secret_key" config.secret_key)
  manufacturer <- either fail pure (traverse (decodeHexText "manufacturer_data") config.manufacturer_data)
  transport <-
    either (fail . show) pure
      =<< connectBluez
        BluezConfig
          { deviceAddress = T.unpack <$> config.mac_address
          , devicePath = objectPath_ . T.unpack <$> config.device_path
          , writeCharacteristicPath = objectPath_ . T.unpack <$> config.write_characteristic_path
          , notifyCharacteristicPath = objectPath_ . T.unpack <$> config.notify_characteristic_path
          , manufacturerData = manufacturer
          , discoveryTimeoutSeconds = 10
          , debugLogging = debugLogging
          }
  sesame <- Sesame.newSesame5ClientWith (sesameClientConfig config) transport
  _ <- Sesame.login sesame (SecretKey secret) `Exception.onException` transport.closeBle
  pure
    ConnectedSesameDevice
      { sesameClient = sesame
      , sesameTransport = transport
      }

deviceUuid :: DeviceConfig -> SesameTransport -> IO UUID.UUID
deviceUuid config transport =
  case config.uuid of
    Just uuidText -> maybe (fail ("invalid Sesame UUID: " <> T.unpack uuidText)) pure (UUID.fromString (T.unpack uuidText))
    Nothing ->
      transport.advertisement >>= \case
        Right advertisement -> pure advertisement.deviceUuid
        Left err -> fail ("failed to discover Sesame UUID from advertisement: " <> show err)

sesameClientConfig :: DeviceConfig -> Sesame.Sesame5ClientConfig
sesameClientConfig config =
  Sesame.defaultSesame5ClientConfig
    { Sesame.commandTimeoutMicros = maybe Sesame.defaultSesame5ClientConfig.commandTimeoutMicros (* 1000) config.command_timeout_ms
    }

decodeHexText :: String -> Text -> Either String ByteString
decodeHexText fieldName source =
  let stripped = T.filter (/= ' ') source
   in if T.length stripped `mod` 2 /= 0 || T.any (not . isHexDigit) stripped
        then Left ("invalid hex in " <> fieldName)
        else Right (BS.pack (go (T.unpack stripped)))
  where
    go (hi : lo : rest) = fromIntegral (digitToInt hi * 16 + digitToInt lo) : go rest
    go [] = []
    go [_] = error "decodeHexText: odd length checked above"
