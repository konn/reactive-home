module Network.Sesame.Transport.Bluez (
  BluezConfig (..),
  defaultBluezConfig,
  connectBluez,
) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket_, try)
import DBus
import DBus.Client qualified as DBus
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Word (Word16, Word8)
import Network.Sesame.Codec
import Network.Sesame.Exception
import Network.Sesame.Transport

data BluezConfig = BluezConfig
  { deviceAddress :: !(Maybe String)
  , devicePath :: !(Maybe ObjectPath)
  , writeCharacteristicPath :: !(Maybe ObjectPath)
  , notifyCharacteristicPath :: !(Maybe ObjectPath)
  , manufacturerData :: !(Maybe ByteString)
  , discoveryTimeoutSeconds :: !Int
  }
  deriving stock (Show, Eq)

defaultBluezConfig :: ObjectPath -> ObjectPath -> ObjectPath -> BluezConfig
defaultBluezConfig device writeChar notifyChar =
  BluezConfig
    { deviceAddress = Nothing
    , devicePath = Just device
    , writeCharacteristicPath = Just writeChar
    , notifyCharacteristicPath = Just notifyChar
    , manufacturerData = Nothing
    , discoveryTimeoutSeconds = 5
    }

connectBluez :: BluezConfig -> IO (Either SesameTransportError SesameTransport)
connectBluez config = do
  result <- try do
    client <- DBus.connectSystem
    resolved <- resolveBluezConfig client config
    queue <- newTQueueIO
    state <- newTVarIO emptyReassembly
    device <- requireField "devicePath" resolved.devicePath
    writeChar <- requireField "writeCharacteristicPath" resolved.writeCharacteristicPath
    notifyChar <- requireField "notifyCharacteristicPath" resolved.notifyCharacteristicPath
    _ <- DBus.addMatch client (propertiesChangedRule notifyChar) (handleSignal queue state)
    callNoBody client device "org.bluez.Device1" "Connect"
    callNoBody client notifyChar "org.bluez.GattCharacteristic1" "StartNotify"
    pure
      SesameTransport
        { sendBle = \encrypted bytes -> sendFragments client writeChar encrypted bytes
        , receiveBle = atomically (readTQueue queue)
        , closeBle = do
            _ <- try @SomeException (callNoBody client notifyChar "org.bluez.GattCharacteristic1" "StopNotify")
            _ <- try @SomeException (callNoBody client device "org.bluez.Device1" "Disconnect")
            DBus.disconnect client
        , advertisement = pure (maybe (Left AdvertisementUnavailable) (either (Left . TransportCallFailed . show) Right . decodeAdvertisement) resolved.manufacturerData)
        }
  pure (either (Left . TransportCallFailed . show @SomeException) Right result)

type BluezProperties = Map String Variant

type BluezInterfaces = Map String BluezProperties

type ManagedObjects = Map ObjectPath BluezInterfaces

sesameWriteCharacteristicUuid :: String
sesameWriteCharacteristicUuid = "16860002-a5ae-9856-b6d3-dbb4c676993e"

sesameNotifyCharacteristicUuid :: String
sesameNotifyCharacteristicUuid = "16860003-a5ae-9856-b6d3-dbb4c676993e"

resolveBluezConfig :: DBus.Client -> BluezConfig -> IO BluezConfig
resolveBluezConfig client config = do
  initialObjects <- getManagedObjects client
  (objects, device) <- case config.devicePath of
    Just path -> pure (initialObjects, path)
    Nothing -> do
      macAddress <- requireField "deviceAddress" config.deviceAddress
      case findDeviceByAddress macAddress initialObjects of
        Just path -> pure (initialObjects, path)
        Nothing -> discoverDevice client config.discoveryTimeoutSeconds macAddress
  callNoBody client device "org.bluez.Device1" "Connect"
  objectsWithServices <-
    if needsCharacteristicDiscovery config
      then waitForCharacteristics client config.discoveryTimeoutSeconds device objects
      else getManagedObjects client
  pure
    config
      { devicePath = Just device
      , writeCharacteristicPath = config.writeCharacteristicPath <|> findCharacteristic sesameWriteCharacteristicUuid device objectsWithServices
      , notifyCharacteristicPath = config.notifyCharacteristicPath <|> findCharacteristic sesameNotifyCharacteristicUuid device objectsWithServices
      , manufacturerData = config.manufacturerData <|> findManufacturerData device objectsWithServices
      }

discoverDevice :: DBus.Client -> Int -> String -> IO (ManagedObjects, ObjectPath)
discoverDevice client timeoutSeconds macAddress = do
  objects <- getManagedObjects client
  adapter <- maybe (fail "BlueZ adapter not found") pure (findAdapter objects)
  bracket_
    (callNoBody client adapter "org.bluez.Adapter1" "StartDiscovery")
    (ignoreErrors (callNoBody client adapter "org.bluez.Adapter1" "StopDiscovery"))
    ( poll timeoutSeconds do
        objects' <- getManagedObjects client
        pure ((objects',) <$> findDeviceByAddress macAddress objects')
    )
    >>= \case
      Just found -> pure found
      Nothing -> fail ("BlueZ device not found for address " <> macAddress)

waitForCharacteristics :: DBus.Client -> Int -> ObjectPath -> ManagedObjects -> IO ManagedObjects
waitForCharacteristics client timeoutSeconds device objects
  | hasCharacteristic sesameWriteCharacteristicUuid device objects && hasCharacteristic sesameNotifyCharacteristicUuid device objects = pure objects
  | otherwise =
      poll timeoutSeconds do
        objects' <- getManagedObjects client
        pure
          if hasCharacteristic sesameWriteCharacteristicUuid device objects' && hasCharacteristic sesameNotifyCharacteristicUuid device objects'
            then Just objects'
            else Nothing
        >>= \case
          Just objects' -> pure objects'
          Nothing -> fail "Sesame GATT characteristics not found in BlueZ"

needsCharacteristicDiscovery :: BluezConfig -> Bool
needsCharacteristicDiscovery config =
  maybe True (const False) config.writeCharacteristicPath
    || maybe True (const False) config.notifyCharacteristicPath

poll :: Int -> IO (Maybe a) -> IO (Maybe a)
poll timeoutSeconds action = go (max 1 timeoutSeconds * 10)
  where
    go attempts = do
      result <- action
      case (attempts, result) of
        (_, Just _) -> pure result
        (0, Nothing) -> pure Nothing
        _ -> threadDelay 100000 *> go (attempts - 1)

getManagedObjects :: DBus.Client -> IO ManagedObjects
getManagedObjects client = do
  reply <-
    DBus.call_ client $
      (methodCall "/" (interfaceName_ "org.freedesktop.DBus.ObjectManager") (memberName_ "GetManagedObjects"))
        { methodCallDestination = Just (busName_ "org.bluez")
        }
  case methodReturnBody reply of
    [body]
      | Just objects <- fromVariant body -> pure objects
    _ -> fail "unexpected BlueZ GetManagedObjects response"

findAdapter :: ManagedObjects -> Maybe ObjectPath
findAdapter = fmap fst . find (Map.member "org.bluez.Adapter1" . snd) . Map.toList

findDeviceByAddress :: String -> ManagedObjects -> Maybe ObjectPath
findDeviceByAddress macAddress =
  fmap fst
    . find
      ( \(_, interfaces) ->
          case Map.lookup "org.bluez.Device1" interfaces >>= lookupProperty "Address" of
            Just deviceAddress -> normalizeAddress deviceAddress == normalizeAddress macAddress
            Nothing -> False
      )
    . Map.toList

findCharacteristic :: String -> ObjectPath -> ManagedObjects -> Maybe ObjectPath
findCharacteristic uuid device =
  fmap fst
    . find
      ( \(path, interfaces) ->
          pathBelongsTo device path
            && case Map.lookup "org.bluez.GattCharacteristic1" interfaces >>= lookupProperty "UUID" of
              Just characteristicUuid -> normalizeUuid characteristicUuid == normalizeUuid uuid
              Nothing -> False
      )
    . Map.toList

hasCharacteristic :: String -> ObjectPath -> ManagedObjects -> Bool
hasCharacteristic uuid device = maybe False (const True) . findCharacteristic uuid device

findManufacturerData :: ObjectPath -> ManagedObjects -> Maybe ByteString
findManufacturerData device objects = do
  interfaces <- Map.lookup device objects
  props <- Map.lookup "org.bluez.Device1" interfaces
  raw <- Map.lookup "ManufacturerData" props
  values <- fromVariant @(Map Word16 Variant) raw
  listToMaybe (filter looksLikeSesameAdvertisement (mapMaybe manufacturerBytes (Map.elems values)))

manufacturerBytes :: Variant -> Maybe ByteString
manufacturerBytes value =
  fromVariant value <|> (BS.pack <$> fromVariant @[Word8] value)

looksLikeSesameAdvertisement :: ByteString -> Bool
looksLikeSesameAdvertisement bytes =
  BS.length bytes == 19 && either (const False) (const True) (decodeAdvertisement bytes)

lookupProperty :: (IsVariant a) => String -> BluezProperties -> Maybe a
lookupProperty name props = Map.lookup name props >>= fromVariant

pathBelongsTo :: ObjectPath -> ObjectPath -> Bool
pathBelongsTo parent child =
  let prefix = formatObjectPath parent <> "/"
   in prefix == take (length prefix) (formatObjectPath child)

normalizeAddress :: String -> String
normalizeAddress = map toLower . filter (/= ':')

normalizeUuid :: String -> String
normalizeUuid = map toLower

requireField :: String -> Maybe a -> IO a
requireField name = maybe (fail ("missing BlueZ " <> name)) pure

ignoreErrors :: IO () -> IO ()
ignoreErrors action = do
  _ <- try @SomeException action
  pure ()

propertiesChangedRule :: ObjectPath -> DBus.MatchRule
propertiesChangedRule path =
  DBus.matchAny
    { DBus.matchPath = Just path
    , DBus.matchInterface = Just (interfaceName_ "org.freedesktop.DBus.Properties")
    , DBus.matchMember = Just (memberName_ "PropertiesChanged")
    }

handleSignal :: TQueue (Either SesameTransportError (ByteString, Bool)) -> TVar Reassembly -> Signal -> IO ()
handleSignal queue state signalMessage =
  case signalBody signalMessage of
    [ifaceVar, changedVar, _invalidatedVar]
      | Just (iface :: String) <- fromVariant ifaceVar
      , iface == "org.bluez.GattCharacteristic1"
      , Just (changed :: Map String Variant) <- fromVariant changedVar
      , Just valueVar <- Map.lookup "Value" changed
      , Just (value :: [Word8]) <- fromVariant valueVar ->
          atomically do
            current <- readTVar state
            case pushFragment current (BS.pack value) of
              Left err -> writeTQueue queue (Left (TransportCallFailed (show err)))
              Right (next, Nothing) -> writeTVar state next
              Right (next, Just complete) -> writeTVar state next *> writeTQueue queue (Right complete)
    _ -> pure ()

sendFragments :: DBus.Client -> ObjectPath -> Bool -> ByteString -> IO (Either SesameTransportError ())
sendFragments client path encrypted bytes = do
  result <- try @SomeException do
    mapM_ (writePacket client path) (fragment encrypted bytes)
  pure (either (Left . TransportCallFailed . show) (const (Right ())) result)

writePacket :: DBus.Client -> ObjectPath -> ByteString -> IO ()
writePacket client path packet =
  let call =
        (methodCall path (interfaceName_ "org.bluez.GattCharacteristic1") (memberName_ "WriteValue"))
          { methodCallDestination = Just (busName_ "org.bluez")
          , methodCallBody = [toVariant (BS.unpack packet :: [Word8]), toVariant (Map.empty :: Map String Variant)]
          }
   in DBus.call_ client call *> pure ()

callNoBody :: DBus.Client -> ObjectPath -> String -> String -> IO ()
callNoBody client path iface member =
  let call =
        (methodCall path (interfaceName_ iface) (memberName_ member))
          { methodCallDestination = Just (busName_ "org.bluez")
          }
   in DBus.call_ client call *> pure ()
