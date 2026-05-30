module Network.Sesame.Transport.Bluez (
  BluezConfig (..),
  defaultBluezConfig,
  connectBluez,
) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Concurrent.STM
import Control.Exception.Safe qualified as Exception
import DBus
import DBus.Client qualified as DBus
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import Data.Word (Word16, Word8)
import Network.Sesame.Codec
import Network.Sesame.Exception
import Network.Sesame.Transport
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

data BluezConfig = BluezConfig
  { deviceAddress :: !(Maybe String)
  , devicePath :: !(Maybe ObjectPath)
  , writeCharacteristicPath :: !(Maybe ObjectPath)
  , notifyCharacteristicPath :: !(Maybe ObjectPath)
  , manufacturerData :: !(Maybe ByteString)
  , discoveryTimeoutSeconds :: !Int
  , debugLogging :: !Bool
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
    , debugLogging = False
    }

connectBluez :: BluezConfig -> IO (Either SesameTransportError SesameTransport)
connectBluez config = do
  result <- Exception.tryAny do
    debug config "connecting to system D-Bus"
    client <- DBus.connectSystem
    Exception.onException (setupBluezTransport client config) (DBus.disconnect client)
  pure (either (Left . TransportCallFailed . show) Right result)

setupBluezTransport :: DBus.Client -> BluezConfig -> IO SesameTransport
setupBluezTransport client config = do
  resolved <- resolveBluezConfig client config
  queue <- newTQueueIO
  state <- newTVarIO emptyReassembly
  lastMessage <- newTVarIO Nothing
  device <- requireField "devicePath" resolved.devicePath
  writeChar <- requireField "writeCharacteristicPath" resolved.writeCharacteristicPath
  notifyChar <- requireField "notifyCharacteristicPath" resolved.notifyCharacteristicPath
  Exception.onException
    do
      debug config ("resolved device=" <> formatObjectPath device <> ", write=" <> formatObjectPath writeChar <> ", notify=" <> formatObjectPath notifyChar)
      _ <- DBus.addMatch client (propertiesChangedRule notifyChar) (handleSignal config queue state lastMessage)
      _ <- DBus.addMatch client (propertiesChangedRule device) (handleDeviceSignal config queue)
      debug config "starting BlueZ notifications"
      callNoBody config.discoveryTimeoutSeconds client notifyChar "org.bluez.GattCharacteristic1" "StartNotify"
      debug config "BlueZ notifications started"
      snapshotNotifyValue client config queue state lastMessage notifyChar
      pure
        SesameTransport
          { sendBle = \encrypted bytes -> sendFragments config client writeChar encrypted bytes
          , receiveBle = atomically (readTQueue queue)
          , closeBle = closeBluezTransport client config device notifyChar
          , advertisement = pure (maybe (Left AdvertisementUnavailable) (either (Left . TransportCallFailed . show) Right . decodeAdvertisement) resolved.manufacturerData)
          }
    (closeBluezTransport client config device notifyChar)

closeBluezTransport :: DBus.Client -> BluezConfig -> ObjectPath -> ObjectPath -> IO ()
closeBluezTransport client config device notifyChar = do
  debug config "closing BlueZ transport"
  _ <- Exception.tryAny (callNoBody config.discoveryTimeoutSeconds client notifyChar "org.bluez.GattCharacteristic1" "StopNotify")
  _ <- Exception.tryAny (callNoBody config.discoveryTimeoutSeconds client device "org.bluez.Device1" "Disconnect")
  DBus.disconnect client

type BluezProperties = Map String Variant

type BluezInterfaces = Map String BluezProperties

type ManagedObjects = Map ObjectPath BluezInterfaces

sesameWriteCharacteristicUuid :: String
sesameWriteCharacteristicUuid = "16860002-a5ae-9856-b6d3-dbb4c676993e"

sesameNotifyCharacteristicUuid :: String
sesameNotifyCharacteristicUuid = "16860003-a5ae-9856-b6d3-dbb4c676993e"

resolveBluezConfig :: DBus.Client -> BluezConfig -> IO BluezConfig
resolveBluezConfig client config = do
  debug config "reading BlueZ managed objects"
  initialObjects <- getManagedObjects client
  (objects, device) <- case config.devicePath of
    Just path -> debug config ("using configured BlueZ device path " <> formatObjectPath path) *> pure (initialObjects, path)
    Nothing -> do
      macAddress <- requireField "deviceAddress" config.deviceAddress
      case findDeviceByAddress macAddress initialObjects of
        Just path -> debug config ("found BlueZ device by address " <> macAddress <> ": " <> formatObjectPath path) *> pure (initialObjects, path)
        Nothing -> do
          debug config ("BlueZ device not found by address " <> macAddress <> "; starting discovery")
          discoverDevice client config.discoveryTimeoutSeconds macAddress
  refreshedObjects <- case config.deviceAddress of
    Just macAddress -> refreshDeviceDiscovery client config macAddress device
    Nothing -> pure objects
  objectsAfterConnect <- connectDevice client config device refreshedObjects
  objectsWithServices <-
    if needsCharacteristicDiscovery config
      then debug config "waiting for Sesame GATT characteristics" *> waitForCharacteristics client config.discoveryTimeoutSeconds device objectsAfterConnect
      else debug config "using configured Sesame GATT characteristic paths" *> getManagedObjects client
  debug config ("advertisement data available=" <> show (maybe False (const True) (config.manufacturerData <|> findAdvertisementData device objectsWithServices)))
  pure
    config
      { devicePath = Just device
      , writeCharacteristicPath = config.writeCharacteristicPath <|> findCharacteristic sesameWriteCharacteristicUuid device objectsWithServices
      , notifyCharacteristicPath = config.notifyCharacteristicPath <|> findCharacteristic sesameNotifyCharacteristicUuid device objectsWithServices
      , manufacturerData = config.manufacturerData <|> findAdvertisementData device objectsWithServices
      }

discoverDevice :: DBus.Client -> Int -> String -> IO (ManagedObjects, ObjectPath)
discoverDevice client timeoutSeconds macAddress = do
  objects <- getManagedObjects client
  adapter <- maybe (fail "BlueZ adapter not found") pure (findAdapter objects)
  Exception.bracket_
    (callNoBody timeoutSeconds client adapter "org.bluez.Adapter1" "StartDiscovery")
    (ignoreErrors (callNoBody timeoutSeconds client adapter "org.bluez.Adapter1" "StopDiscovery"))
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

refreshDeviceDiscovery :: DBus.Client -> BluezConfig -> String -> ObjectPath -> IO ManagedObjects
refreshDeviceDiscovery client config macAddress device = do
  objects <- getManagedObjects client
  adapter <- maybe (fail "BlueZ adapter not found") pure (findAdapter objects)
  debug config ("refreshing BlueZ discovery for " <> macAddress)
  Exception.bracket_
    (callNoBody config.discoveryTimeoutSeconds client adapter "org.bluez.Adapter1" "StartDiscovery")
    (ignoreErrors (callNoBody config.discoveryTimeoutSeconds client adapter "org.bluez.Adapter1" "StopDiscovery"))
    do
      threadDelay bluezDiscoveryRefreshMicros
      refreshed <- getManagedObjects client
      case findDeviceByAddress macAddress refreshed of
        Just refreshedDevice
          | refreshedDevice == device -> pure refreshed
          | otherwise -> do
              debug config ("BlueZ device path changed after discovery: " <> formatObjectPath refreshedDevice)
              pure refreshed
        Nothing -> fail ("BlueZ device disappeared during discovery refresh: " <> macAddress)

connectDevice :: DBus.Client -> BluezConfig -> ObjectPath -> ManagedObjects -> IO ManagedObjects
connectDevice client config device objects = do
  resetStaleDeviceConnection client config device objects
  debug config ("connecting BlueZ device " <> formatObjectPath device)
  Exception.onException
    (callNoBody config.discoveryTimeoutSeconds client device "org.bluez.Device1" "Connect")
    do
      debug config ("cancelling failed BlueZ connect " <> formatObjectPath device)
      ignoreErrors (callNoBody config.discoveryTimeoutSeconds client device "org.bluez.Device1" "Disconnect")
  waitForServicesResolved client config.discoveryTimeoutSeconds device

resetStaleDeviceConnection :: DBus.Client -> BluezConfig -> ObjectPath -> ManagedObjects -> IO ()
resetStaleDeviceConnection client config device objects =
  if isDeviceConnected device objects
    then do
      debug config ("disconnecting stale BlueZ device session " <> formatObjectPath device)
      ignoreErrors (callNoBody config.discoveryTimeoutSeconds client device "org.bluez.Device1" "Disconnect")
      disconnected <-
        poll config.discoveryTimeoutSeconds do
          objects' <- getManagedObjects client
          pure
            if isDeviceConnected device objects'
              then Nothing
              else Just ()
      maybe
        (debug config "timed out waiting for BlueZ disconnect; continuing with connect")
        (const (debug config "BlueZ device disconnected before reconnect"))
        disconnected
    else pure ()

waitForServicesResolved :: DBus.Client -> Int -> ObjectPath -> IO ManagedObjects
waitForServicesResolved client timeoutSeconds device = do
  resolved <-
    poll timeoutSeconds do
      objects <- getManagedObjects client
      pure do
        if isDeviceConnected device objects && areDeviceServicesResolved device objects
          then Just objects
          else Nothing
  maybe (fail "BlueZ device services were not resolved") pure resolved

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

isDeviceConnected :: ObjectPath -> ManagedObjects -> Bool
isDeviceConnected device objects =
  maybe False id do
    interfaces <- Map.lookup device objects
    props <- Map.lookup "org.bluez.Device1" interfaces
    lookupProperty "Connected" props

areDeviceServicesResolved :: ObjectPath -> ManagedObjects -> Bool
areDeviceServicesResolved device objects =
  maybe False id do
    interfaces <- Map.lookup device objects
    props <- Map.lookup "org.bluez.Device1" interfaces
    lookupProperty "ServicesResolved" props

findAdvertisementData :: ObjectPath -> ManagedObjects -> Maybe ByteString
findAdvertisementData device objects =
  findManufacturerData device objects <|> findServiceData device objects

findManufacturerData :: ObjectPath -> ManagedObjects -> Maybe ByteString
findManufacturerData device objects = do
  interfaces <- Map.lookup device objects
  props <- Map.lookup "org.bluez.Device1" interfaces
  raw <- Map.lookup "ManufacturerData" props
  values <- fromVariant @(Map Word16 Variant) raw
  listToMaybe (filter looksLikeSesameAdvertisement (mapMaybe manufacturerBytes (Map.elems values)))

findServiceData :: ObjectPath -> ManagedObjects -> Maybe ByteString
findServiceData device objects = do
  interfaces <- Map.lookup device objects
  props <- Map.lookup "org.bluez.Device1" interfaces
  raw <- Map.lookup "ServiceData" props
  values <- fromVariant @(Map String Variant) raw
  listToMaybe (filter looksLikeSesameAdvertisement (mapMaybe advertisementBytes (Map.elems values)))

manufacturerBytes :: Variant -> Maybe ByteString
manufacturerBytes = advertisementBytes

advertisementBytes :: Variant -> Maybe ByteString
advertisementBytes value =
  fromVariant value <|> (BS.pack <$> fromVariant @[Word8] value)

variantBytes :: Variant -> Maybe ByteString
variantBytes = advertisementBytes

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
  _ <- Exception.tryAny action
  pure ()

propertiesChangedRule :: ObjectPath -> DBus.MatchRule
propertiesChangedRule path =
  DBus.matchAny
    { DBus.matchPath = Just path
    , DBus.matchInterface = Just (interfaceName_ "org.freedesktop.DBus.Properties")
    , DBus.matchMember = Just (memberName_ "PropertiesChanged")
    }

handleSignal :: BluezConfig -> TQueue (Either SesameTransportError (ByteString, Bool)) -> TVar Reassembly -> TVar (Maybe (ByteString, Bool)) -> Signal -> IO ()
handleSignal config queue state lastMessage signalMessage =
  case signalBody signalMessage of
    [ifaceVar, changedVar, _invalidatedVar]
      | Just (iface :: String) <- fromVariant ifaceVar
      , iface == "org.bluez.GattCharacteristic1"
      , Just (changed :: Map String Variant) <- fromVariant changedVar
      , Just valueVar <- Map.lookup "Value" changed
      , Just bytes <- variantBytes valueVar ->
          processNotificationBytes config queue state lastMessage "notification" bytes
      | Just (iface :: String) <- fromVariant ifaceVar
      , iface == "org.bluez.GattCharacteristic1"
      , Just (changed :: Map String Variant) <- fromVariant changedVar ->
          debug config ("GattCharacteristic1 change without decodable Value: keys=" <> show (Map.keys changed))
    _ -> pure ()

snapshotNotifyValue :: DBus.Client -> BluezConfig -> TQueue (Either SesameTransportError (ByteString, Bool)) -> TVar Reassembly -> TVar (Maybe (ByteString, Bool)) -> ObjectPath -> IO ()
snapshotNotifyValue client config queue state lastMessage notifyChar = do
  result <- Exception.tryAny (readCharacteristicValue client notifyChar)
  case result of
    Right (Just bytes)
      | not (BS.null bytes) -> processNotificationBytes config queue state lastMessage "notification snapshot" bytes
    Right _ -> pure ()
    Left err -> debug config ("failed to snapshot BlueZ notification value: " <> show err)

processNotificationBytes :: BluezConfig -> TQueue (Either SesameTransportError (ByteString, Bool)) -> TVar Reassembly -> TVar (Maybe (ByteString, Bool)) -> String -> ByteString -> IO ()
processNotificationBytes config queue state lastMessage label bytes = do
  event <-
    atomically do
      current <- readTVar state
      case pushFragment current bytes of
        Left err -> writeTQueue queue (Left (TransportCallFailed (show err))) *> pure (label <> " reassembly failed: " <> show err)
        Right (next, Nothing) -> writeTVar state next *> pure (label <> " fragment received: bytes=" <> show (BS.length bytes))
        Right (next, Just message@(payload, encrypted)) -> do
          writeTVar state next
          previous <- readTVar lastMessage
          if previous == Just message
            then pure (label <> " duplicate message ignored: payload_bytes=" <> show (BS.length payload) <> ", encrypted=" <> show encrypted)
            else
              writeTVar lastMessage (Just message)
                *> writeTQueue queue (Right message)
                *> pure (label <> " message received: payload_bytes=" <> show (BS.length payload) <> ", encrypted=" <> show encrypted)
  debug config event

readCharacteristicValue :: DBus.Client -> ObjectPath -> IO (Maybe ByteString)
readCharacteristicValue client path = do
  reply <-
    DBus.call_ client $
      (methodCall path (interfaceName_ "org.freedesktop.DBus.Properties") (memberName_ "Get"))
        { methodCallDestination = Just (busName_ "org.bluez")
        , methodCallBody =
            [ toVariant ("org.bluez.GattCharacteristic1" :: String)
            , toVariant ("Value" :: String)
            ]
        }
  case methodReturnBody reply of
    [body]
      | Just value <- fromVariant body -> pure (variantBytes value)
    _ -> fail "unexpected BlueZ Properties.Get Value response"

handleDeviceSignal :: BluezConfig -> TQueue (Either SesameTransportError (ByteString, Bool)) -> Signal -> IO ()
handleDeviceSignal config queue signalMessage =
  case signalBody signalMessage of
    [ifaceVar, changedVar, _invalidatedVar]
      | Just (iface :: String) <- fromVariant ifaceVar
      , iface == "org.bluez.Device1"
      , Just (changed :: Map String Variant) <- fromVariant changedVar
      , Just connectedVar <- Map.lookup "Connected" changed
      , Just (connected :: Bool) <- fromVariant connectedVar
      , not connected ->
          debug config "BlueZ device disconnected" *> atomically (writeTQueue queue (Left TransportClosed))
    _ -> pure ()

sendFragments :: BluezConfig -> DBus.Client -> ObjectPath -> Bool -> ByteString -> IO (Either SesameTransportError ())
sendFragments config client path encrypted bytes = do
  let packets = fragment encrypted bytes
  debug config ("writing BLE message: payload_bytes=" <> show (BS.length bytes) <> ", encrypted=" <> show encrypted <> ", fragments=" <> show (length packets))
  result <- Exception.tryAny do
    mapM_ (writePacket config.discoveryTimeoutSeconds client path) packets
  pure (either (Left . TransportCallFailed . show) (const (Right ())) result)

writePacket :: Int -> DBus.Client -> ObjectPath -> ByteString -> IO ()
writePacket timeoutSeconds client path packet =
  let call =
        (methodCall path (interfaceName_ "org.bluez.GattCharacteristic1") (memberName_ "WriteValue"))
          { methodCallDestination = Just (busName_ "org.bluez")
          , methodCallBody =
              [ toVariant (BS.unpack packet :: [Word8])
              , toVariant (Map.singleton "type" (toVariant ("command" :: String)) :: Map String Variant)
              ]
          }
      label = "org.bluez.GattCharacteristic1.WriteValue " <> formatObjectPath path
   in maybe (fail ("BlueZ D-Bus call timed out: " <> label)) (const (pure ())) =<< timeout (callTimeoutMicros timeoutSeconds) (DBus.call_ client call)

callNoBody :: Int -> DBus.Client -> ObjectPath -> String -> String -> IO ()
callNoBody timeoutSeconds client path iface member =
  let call =
        (methodCall path (interfaceName_ iface) (memberName_ member))
          { methodCallDestination = Just (busName_ "org.bluez")
          }
      label = iface <> "." <> member <> " " <> formatObjectPath path
   in maybe (fail ("BlueZ D-Bus call timed out: " <> label)) (const (pure ())) =<< timeout (callTimeoutMicros timeoutSeconds) (DBus.call_ client call)

callTimeoutMicros :: Int -> Int
callTimeoutMicros timeoutSeconds = max 1 timeoutSeconds * 1000000

bluezDiscoveryRefreshMicros :: Int
bluezDiscoveryRefreshMicros = 1500000

debug :: BluezConfig -> String -> IO ()
debug config message =
  if config.debugLogging
    then do
      timestamp <- currentTimestamp
      withDebugLock (hPutStrLn stderr (timestamp <> " [haskesame-transport-bluez] " <> message))
    else pure ()

currentTimestamp :: IO String
currentTimestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q %Z" <$> getZonedTime

withDebugLock :: IO () -> IO ()
withDebugLock action = modifyMVar_ debugLock \() -> action *> pure ()

debugLock :: MVar ()
debugLock = unsafePerformIO (newMVar ())
{-# NOINLINE debugLock #-}
