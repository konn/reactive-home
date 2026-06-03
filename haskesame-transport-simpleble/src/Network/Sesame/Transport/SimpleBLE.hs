{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Network.Sesame.Transport.SimpleBLE (
  SimpleBLEConfig (..),
  defaultSimpleBLEConfig,
  connectSimpleBLE,
) where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception.Safe qualified as Exception
import Control.Monad (filterM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import GHC.Generics (Generic)
import Network.Sesame.Codec
import Network.Sesame.Exception
import Network.Sesame.Transport
import Network.Sesame.Types (Advertisement (..))
import SimpleBLE qualified

data SimpleBLEConfig = SimpleBLEConfig
  { deviceAddress :: !Text
  , deviceUuid :: !(Maybe UUID.UUID)
  , scanTimeoutMs :: !Int
  , serviceUuid :: !Text
  , writeCharacteristicUuid :: !(Maybe Text)
  , notifyCharacteristicUuid :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Generic)

data DiscoveredPeripheral = DiscoveredPeripheral
  { peripheral :: !SimpleBLE.Peripheral
  , advertisementData :: !(Maybe ByteString)
  }

defaultSimpleBLEConfig :: Text -> SimpleBLEConfig
defaultSimpleBLEConfig address =
  SimpleBLEConfig
    { deviceAddress = address
    , deviceUuid = Nothing
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
  result <- Exception.tryAny do
    discovered <- discoverPeripheral config
    let peripheral = discovered.peripheral
    SimpleBLE.peripheralConnect peripheral
    services <- SimpleBLE.peripheralServices peripheral
    writeCharacteristic <- requireCharacteristic "write" (config.writeCharacteristicUuid <|> discoverWritableCharacteristic config.serviceUuid services)
    notifyCharacteristic <- requireCharacteristic "notify" (config.notifyCharacteristicUuid <|> discoverNotifyCharacteristic config.serviceUuid services)
    advertisementData <- (discovered.advertisementData <|>) <$> peripheralAdvertisementData peripheral
    queue <- newTQueueIO
    state <- newTVarIO emptyReassembly
    closed <- newTVarIO False
    subscription <- SimpleBLE.peripheralNotify peripheral config.serviceUuid notifyCharacteristic (handleNotify queue state)
    monitor <- forkIO (monitorConnection peripheral closed queue)
    pure
      SesameTransport
        { sendBle = \encrypted bytes -> sendFragments peripheral config.serviceUuid writeCharacteristic encrypted bytes
        , receiveBle = atomically (readTQueue queue)
        , closeBle = do
            atomically (writeTVar closed True)
            killThread monitor
            _ <- Exception.tryAny (SimpleBLE.subscriptionUnsubscribe subscription)
            _ <- Exception.tryAny (SimpleBLE.peripheralDisconnect peripheral)
            pure ()
        , advertisement = pure (maybe (Left AdvertisementUnavailable) (either (Left . TransportCallFailed . show) Right . decodeAdvertisement) advertisementData)
        }
  pure (either (Left . TransportCallFailed . show) Right result)

discoverPeripheral :: SimpleBLEConfig -> IO DiscoveredPeripheral
discoverPeripheral config = do
  adapters <- SimpleBLE.getAdapters
  case adapters of
    [] -> fail "SimpleBLE adapter not found"
    adapter : _ -> do
      SimpleBLE.adapterScanFor adapter config.scanTimeoutMs
      peripherals <- SimpleBLE.adapterScanGetResults adapter
      findPeripheral config peripherals >>= maybe (fail =<< deviceNotFoundMessage config peripherals) pure

findPeripheral :: SimpleBLEConfig -> [SimpleBLE.Peripheral] -> IO (Maybe DiscoveredPeripheral)
findPeripheral config peripherals = do
  matches <-
    filterM
      ( \discovered -> do
          peripheralAddress <- SimpleBLE.peripheralAddress discovered.peripheral
          let advertisedUuid = (.deviceUuid) <$> (discovered.advertisementData >>= either (const Nothing) Just . decodeAdvertisement)
          pure
            ( normalizeAddress peripheralAddress == normalizeAddress config.deviceAddress
                || maybe False (\expected -> advertisedUuid == Just expected) config.deviceUuid
            )
      )
      =<< traverse discoverAdvertisementData peripherals
  pure (listToMaybe matches)

discoverAdvertisementData :: SimpleBLE.Peripheral -> IO DiscoveredPeripheral
discoverAdvertisementData peripheral = do
  advertisementData <- peripheralAdvertisementData peripheral
  pure
    DiscoveredPeripheral
      { peripheral = peripheral
      , advertisementData = advertisementData
      }

peripheralAdvertisementData :: SimpleBLE.Peripheral -> IO (Maybe ByteString)
peripheralAdvertisementData peripheral =
  (<|>)
    <$> (findSesameManufacturerData <$> safePeripheralManufacturerData peripheral)
    <*> (findSesameServiceData <$> safePeripheralServices peripheral)

peripheralAdvertisement :: SimpleBLE.Peripheral -> IO (Maybe Advertisement)
peripheralAdvertisement peripheral =
  peripheralAdvertisementData peripheral >>= \case
    Nothing -> pure Nothing
    Just bytes -> pure (either (const Nothing) Just (decodeAdvertisement bytes))

safePeripheralManufacturerData :: SimpleBLE.Peripheral -> IO [SimpleBLE.ManufacturerData]
safePeripheralManufacturerData peripheral =
  either (const []) id <$> Exception.tryAny (SimpleBLE.peripheralManufacturerData peripheral)

safePeripheralServices :: SimpleBLE.Peripheral -> IO [SimpleBLE.Service]
safePeripheralServices peripheral =
  either (const []) id <$> Exception.tryAny (SimpleBLE.peripheralServices peripheral)

deviceNotFoundMessage :: SimpleBLEConfig -> [SimpleBLE.Peripheral] -> IO String
deviceNotFoundMessage config peripherals = do
  discovered <- traverse describePeripheral peripherals
  pure
    ( "SimpleBLE device not found for address "
        <> show config.deviceAddress
        <> maybe "" ((", UUID " <>) . UUID.toString) config.deviceUuid
        <> ". Discovered peripherals: "
        <> if null discovered then "<none>" else T.unpack (T.intercalate "; " discovered)
    )

describePeripheral :: SimpleBLE.Peripheral -> IO Text
describePeripheral peripheral = do
  identifier <- SimpleBLE.peripheralIdentifier peripheral
  address <- SimpleBLE.peripheralAddress peripheral
  advertisement <- peripheralAdvertisement peripheral
  pure
    ( "identifier="
        <> identifier
        <> ", address="
        <> address
        <> maybe "" ((", sesame_uuid=" <>) . T.pack . UUID.toString . (.deviceUuid)) advertisement
    )

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

findSesameServiceData :: [SimpleBLE.Service] -> Maybe ByteString
findSesameServiceData =
  fmap (.data_) . find (\service -> isSesameService service.uuid && looksLikeSesameAdvertisement service.data_)

isSesameService :: Text -> Bool
isSesameService uuid =
  normalizeUuid uuid == normalizeUuid sesameServiceUuid || normalizeUuid uuid == shortUuid sesameServiceUuid

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

monitorConnection :: SimpleBLE.Peripheral -> TVar Bool -> TQueue (Either SesameTransportError (ByteString, Bool)) -> IO ()
monitorConnection peripheral closed queue = go
  where
    go = do
      threadDelay 1000000
      isClosed <- readTVarIO closed
      if isClosed
        then pure ()
        else do
          connected <- either (const False) id <$> Exception.tryAny (SimpleBLE.peripheralIsConnected peripheral)
          if connected
            then go
            else atomically (writeTQueue queue (Left TransportClosed))

sendFragments :: SimpleBLE.Peripheral -> Text -> Text -> Bool -> ByteString -> IO (Either SesameTransportError ())
sendFragments peripheral service characteristic encrypted bytes = do
  result <- Exception.tryAny do
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
