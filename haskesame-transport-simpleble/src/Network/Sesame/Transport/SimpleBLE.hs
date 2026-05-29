{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Network.Sesame.Transport.SimpleBLE (
  SimpleBLEConfig (..),
  defaultSimpleBLEConfig,
  connectSimpleBLE,
) where

import Control.Applicative ((<|>))
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (filterM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Network.Sesame.Codec
import Network.Sesame.Exception
import Network.Sesame.Transport
import SimpleBLE qualified

data SimpleBLEConfig = SimpleBLEConfig
  { deviceAddress :: !Text
  , scanTimeoutMs :: !Int
  , serviceUuid :: !Text
  , writeCharacteristicUuid :: !(Maybe Text)
  , notifyCharacteristicUuid :: !(Maybe Text)
  }
  deriving stock (Show, Eq)

defaultSimpleBLEConfig :: Text -> SimpleBLEConfig
defaultSimpleBLEConfig address =
  SimpleBLEConfig
    { deviceAddress = address
    , scanTimeoutMs = 5000
    , serviceUuid = sesameServiceUuid
    , writeCharacteristicUuid = Nothing
    , notifyCharacteristicUuid = Nothing
    }

sesameServiceUuid :: Text
sesameServiceUuid = "0000fd81-0000-1000-8000-00805f9b34fb"

sesameWriteCharacteristicUuid :: Text
sesameWriteCharacteristicUuid = "16860002-a5ae-9856-b6d3-dbb4c676993e"

sesameNotifyCharacteristicUuid :: Text
sesameNotifyCharacteristicUuid = "16860003-a5ae-9856-b6d3-dbb4c676993e"

connectSimpleBLE :: SimpleBLEConfig -> IO (Either SesameTransportError SesameTransport)
connectSimpleBLE config = do
  result <- try do
    peripheral <- discoverPeripheral config
    SimpleBLE.peripheralConnect peripheral
    services <- SimpleBLE.peripheralServices peripheral
    writeCharacteristic <- requireCharacteristic "write" (config.writeCharacteristicUuid <|> discoverWritableCharacteristic config.serviceUuid services)
    notifyCharacteristic <- requireCharacteristic "notify" (config.notifyCharacteristicUuid <|> discoverNotifyCharacteristic config.serviceUuid services)
    manufacturerData <- findSesameManufacturerData <$> SimpleBLE.peripheralManufacturerData peripheral
    queue <- newTQueueIO
    state <- newTVarIO emptyReassembly
    subscription <- SimpleBLE.peripheralNotify peripheral config.serviceUuid notifyCharacteristic (handleNotify queue state)
    pure
      SesameTransport
        { sendBle = \encrypted bytes -> sendFragments peripheral config.serviceUuid writeCharacteristic encrypted bytes
        , receiveBle = atomically (readTQueue queue)
        , closeBle = do
            _ <- try @SomeException (SimpleBLE.subscriptionUnsubscribe subscription)
            _ <- try @SomeException (SimpleBLE.peripheralDisconnect peripheral)
            pure ()
        , advertisement = pure (maybe (Left AdvertisementUnavailable) (either (Left . TransportCallFailed . show) Right . decodeAdvertisement) manufacturerData)
        }
  pure (either (Left . TransportCallFailed . show @SomeException) Right result)

discoverPeripheral :: SimpleBLEConfig -> IO SimpleBLE.Peripheral
discoverPeripheral config = do
  adapters <- SimpleBLE.getAdapters
  case adapters of
    [] -> fail "SimpleBLE adapter not found"
    adapter : _ -> do
      SimpleBLE.adapterScanFor adapter config.scanTimeoutMs
      peripherals <- SimpleBLE.adapterScanGetResults adapter
      findByAddress config.deviceAddress peripherals >>= maybe (fail ("SimpleBLE device not found for address " <> show config.deviceAddress)) pure

findByAddress :: Text -> [SimpleBLE.Peripheral] -> IO (Maybe SimpleBLE.Peripheral)
findByAddress address peripherals = do
  matches <-
    filterM
      ( \peripheral -> do
          peripheralAddress <- SimpleBLE.peripheralAddress peripheral
          pure (normalizeAddress peripheralAddress == normalizeAddress address)
      )
      peripherals
  pure (listToMaybe matches)

discoverWritableCharacteristic :: Text -> [SimpleBLE.Service] -> Maybe Text
discoverWritableCharacteristic serviceUuid services = do
  service <- findSesameService serviceUuid services
  characteristic <- find (\c -> c.canWriteRequest && normalizeUuid c.uuid == normalizeUuid sesameWriteCharacteristicUuid) service.characteristics
  pure characteristic.uuid

discoverNotifyCharacteristic :: Text -> [SimpleBLE.Service] -> Maybe Text
discoverNotifyCharacteristic serviceUuid services = do
  service <- findSesameService serviceUuid services
  characteristic <- find (\c -> c.canNotify && normalizeUuid c.uuid == normalizeUuid sesameNotifyCharacteristicUuid) service.characteristics
  pure characteristic.uuid

findSesameService :: Text -> [SimpleBLE.Service] -> Maybe SimpleBLE.Service
findSesameService serviceUuid =
  find (\service -> normalizeUuid service.uuid == normalizeUuid serviceUuid || normalizeUuid service.uuid == shortUuid serviceUuid)

findSesameManufacturerData :: [SimpleBLE.ManufacturerData] -> Maybe ByteString
findSesameManufacturerData =
  fmap (.payload) . find (looksLikeSesameAdvertisement . (.payload))

looksLikeSesameAdvertisement :: ByteString -> Bool
looksLikeSesameAdvertisement bytes =
  BS.length bytes == 19 && either (const False) (const True) (decodeAdvertisement bytes)

handleNotify :: TQueue (Either SesameTransportError (ByteString, Bool)) -> TVar Reassembly -> ByteString -> IO ()
handleNotify queue state bytes =
  atomically do
    current <- readTVar state
    case pushFragment current bytes of
      Left err -> writeTQueue queue (Left (TransportCallFailed (show err)))
      Right (next, Nothing) -> writeTVar state next
      Right (next, Just complete) -> writeTVar state next *> writeTQueue queue (Right complete)

sendFragments :: SimpleBLE.Peripheral -> Text -> Text -> Bool -> ByteString -> IO (Either SesameTransportError ())
sendFragments peripheral service characteristic encrypted bytes = do
  result <- try @SomeException do
    mapM_ (SimpleBLE.peripheralWriteRequest peripheral service characteristic) (fragment encrypted bytes)
  pure (either (Left . TransportCallFailed . show) (const (Right ())) result)

requireCharacteristic :: String -> Maybe Text -> IO Text
requireCharacteristic label = maybe (fail ("SimpleBLE Sesame " <> label <> " characteristic not found")) pure

normalizeAddress :: Text -> Text
normalizeAddress = lowerAscii . withoutSeparators

normalizeUuid :: Text -> Text
normalizeUuid = lowerAscii

shortUuid :: Text -> Text
shortUuid uuid =
  let normalized = normalizeUuid uuid
   in if T.length normalized == 36 && T.drop 8 normalized == "-0000-1000-8000-00805f9b34fb"
        then T.take 4 (T.drop 4 normalized)
        else normalized

lowerAscii :: Text -> Text
lowerAscii = T.map toLower

withoutSeparators :: Text -> Text
withoutSeparators = T.filter (/= ':')
