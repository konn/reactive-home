-- | Advertisement-only scanning through the Linux BlueZ D-Bus API.
module Network.SwitchBot.Bluez (
  scanSensors,
  scanSensorsWithClient,
  withScanner,
  withScannerWithClient,
  withScannerWithClientAndClock,
  DiscoverySilence (..),
  DiscoveryFailure (..),
  isDiscoveryFailure,
  recoverDiscovery,
  recoverDiscoveryWithClient,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVar, writeTVar)
import Control.Exception.Safe (Exception, SomeException, bracket, bracketOnError, finally, fromException, mask, onException, throwIO, tryAny)
import Control.Monad (unless, void, when)
import DBus
import DBus.Client qualified as DBus
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Clock (getMonotonicTimeNSec)
import Network.SwitchBot.Advertisement (SensorReading, normalizeDeviceId)
import Network.SwitchBot.Bluez.Advertisement
import System.Timeout (timeout)

{- | A discovery error for which a sustained failure can justify adapter recovery.
Permission, configuration and powered-off errors are deliberately excluded.
-}
data DiscoveryFailure = DiscoveryFailure String
  deriving stock (Show)

instance Exception DiscoveryFailure

{- | No advertising signals despite Discovering=true. Renew the discovery
session, but do not classify mere radio silence as an adapter reset condition.
-}
data DiscoverySilence = DiscoverySilence
  deriving stock (Show)

instance Exception DiscoverySilence

monotonicSeconds :: IO Double
monotonicSeconds = (/ 1000000000) . fromIntegral <$> getMonotonicTimeNSec

-- DBus.call_ discards the error name; retain it for precise recovery decisions.
newtype BluezCallError = BluezCallError MethodError
  deriving stock (Show)

instance Exception BluezCallError

isDiscoveryFailure :: SomeException -> Bool
isDiscoveryFailure err = case fromException @DiscoveryFailure err of
  Just _ -> True
  Nothing -> False

-- | One-shot compatibility API. Long-running consumers should use 'withScanner'.
scanSensors :: Maybe Text -> Int -> (String -> IO ()) -> IO [SensorReading]
scanSensors requested milliseconds report = withScanner requested report ($ milliseconds)

scanSensorsWithClient :: DBus.Client -> Maybe Text -> Int -> (String -> IO ()) -> IO [SensorReading]
scanSensorsWithClient client requested milliseconds report =
  withScannerWithClient client requested report ($ milliseconds)

{- | Own one discovery session across sampling windows, reconnecting lazily after
an error. The supplied scan action has one consumer and must stay within this
scope. Failed/cancelled reads discard the connection and all cached readings.
-}
withScanner :: Maybe Text -> (String -> IO ()) -> ((Int -> IO [SensorReading]) -> IO a) -> IO a
withScanner = withScannerConnection DBus.connectSystem DBus.disconnect monotonicSeconds

-- | Private-bus injection. The caller owns a dedicated D-Bus connection.
withScannerWithClient :: DBus.Client -> Maybe Text -> (String -> IO ()) -> ((Int -> IO [SensorReading]) -> IO a) -> IO a
withScannerWithClient client = withScannerWithClientAndClock client monotonicSeconds

-- | Inject monotonic seconds for deterministic inactivity tests on a private bus.
withScannerWithClientAndClock :: DBus.Client -> IO Double -> Maybe Text -> (String -> IO ()) -> ((Int -> IO [SensorReading]) -> IO a) -> IO a
withScannerWithClientAndClock client = withScannerConnection (pure client) (const $ pure ())

withScannerConnection :: IO DBus.Client -> (DBus.Client -> IO ()) -> IO Double -> Maybe Text -> (String -> IO ()) -> ((Int -> IO [SensorReading]) -> IO a) -> IO a
withScannerConnection connect disconnect now requested report use = do
  current <- newIORef Nothing
  -- Survives session replacement; reset after a renewal request so an actually
  -- quiet room renews at most once a minute, rather than once per empty window.
  lastActivity <- now >>= newIORef
  let close = do
        previous <- atomicModifyIORef' current (Nothing,)
        maybe (pure ()) (\(_, release) -> release) previous
      scan milliseconds = do
        unless (milliseconds > 0 && milliseconds <= 60000) $ fail "Scan window must be between 1 and 60000 milliseconds"
        mask $ \restore ->
          ( do
              previous <- readIORef current
              (readWindow, _) <- case previous of
                Just session -> pure session
                Nothing -> do
                  session <- bracketOnError connect disconnect $ \client -> do
                    (readWindow, stop) <- openDiscovery client requested report
                    pure (readWindow, stop `finally` disconnect client)
                  writeIORef current $ Just session
                  pure session
              window <- restore $ readWindow milliseconds
              observed <- now
              previousActivity <- readIORef lastActivity
              if scanHasActivity window
                then writeIORef lastActivity observed
                else when (observed - previousActivity >= 60) $ do
                  writeIORef lastActivity observed
                  report "SwitchBot discovery silent for 60 seconds; renewing discovery session"
                  throwIO DiscoverySilence
              pure $ scanResults window
          )
            `onException` close
  bracket (pure ()) (const close) $ const $ use scan

-- Acquire under masking; every partial acquisition has a cleanup path.
openDiscovery :: DBus.Client -> Maybe Text -> (String -> IO ()) -> IO (Int -> IO ScanState, IO ())
openDiscovery client requested report = do
  (owner, objects) <- getManagedObjects client
  adapter <- maybe (fail "No matching powered BlueZ adapter") pure $ findAdapter requested objects
  state <- newTVarIO $ initialScan adapter objects
  let receive event = atomically $ modifyTVar' state $ collectSignal adapter event
      add rule = bounded "AddMatch" $ DBus.addMatch client rule receive
      remove = bounded "RemoveMatch" . DBus.removeMatch client
      adapterCall member body = void $ callBoundedTo owner client adapter "org.bluez.Adapter1" member body
      stop = tryAny (adapterCall "StopDiscovery" []) >>= either (report . show) pure
      filterOptions =
        Map.fromList
          [ ("Transport", toVariant ("le" :: String))
          , ("DuplicateData", toVariant True)
          ] ::
          Map.Map String Variant
  handlers <- newIORef []
  let register rule = do
        handler <- add rule
        atomicModifyIORef' handlers $ \previous -> (handler : previous, ())
      removeAll = do
        registered <- atomicModifyIORef' handlers ([],)
        -- Try every release even if the daemon disappeared during cleanup.
        mapM_ (\handler -> tryAny (remove handler) >>= either (report . show) pure) registered
  ( do
      register $ signalRule owner "org.freedesktop.DBus.Properties" "PropertiesChanged"
      register $ signalRule owner "org.freedesktop.DBus.ObjectManager" "InterfacesAdded"
      register $ signalRule owner "org.freedesktop.DBus.ObjectManager" "InterfacesRemoved"
      adapterCall "SetDiscoveryFilter" [toVariant filterOptions]
      (adapterCall "StartDiscovery" [] `catchDiscoveryFailure` "StartDiscovery") `onException` stop
      let readWindow milliseconds = do
            threadDelay $ milliseconds * 1000
            -- A replacement daemon cannot make a session bound to the old owner healthy.
            powered <- getAdapterBool owner client adapter "Powered"
            unless powered $ fail "BlueZ adapter is powered off"
            discovering <- getAdapterBool owner client adapter "Discovering"
            unless discovering $ throwIO $ DiscoveryFailure "BlueZ discovery stopped unexpectedly"
            result <- atomically $ do
              window <- readTVar state
              writeTVar state $ nextScanWindow window
              pure window
            mapM_ (report . show) $ scanErrors result
            pure result
      pure (readWindow, stop `finally` removeAll)
    )
    `onException` removeAll

catchDiscoveryFailure :: IO a -> String -> IO a
catchDiscoveryFailure action label = do
  result <- tryAny action
  case result of
    Left err
      | Just (BluezCallError dbusError) <- fromException @BluezCallError err
      , methodErrorName dbusError `elem` ["org.bluez.Error.InProgress", "org.bluez.Error.Failed"] ->
          throwIO $ DiscoveryFailure $ label <> ": " <> show err
      | otherwise -> throwIO err
    Right value -> pure value

getAdapterBool :: BusName -> DBus.Client -> ObjectPath -> String -> IO Bool
getAdapterBool owner client adapter property = do
  reply <-
    callBoundedTo
      owner
      client
      adapter
      "org.freedesktop.DBus.Properties"
      "Get"
      [toVariant ("org.bluez.Adapter1" :: String), toVariant property]
  case reply.methodReturnBody of
    [body] | Just value <- fromVariant @Variant body >>= fromVariant @Bool -> pure value
    _ -> fail $ "Invalid BlueZ adapter property: " <> property

{- | Reset the selected adapter's shared discovery state. This interrupts GATT
connections on that adapter, including Sesame. Call only after sustained
DiscoveryFailure errors, with a cooldown. It uses BlueZ D-Bus permissions and
never invokes sudo or changes another adapter. Power-on is attempted even if
power-off fails or the recovery is cancelled.
-}
recoverDiscovery :: Maybe Text -> (String -> IO ()) -> IO ()
recoverDiscovery requested report =
  bracket DBus.connectSystem DBus.disconnect $ \client -> recoverDiscoveryWithClient client requested report

recoverDiscoveryWithClient :: DBus.Client -> Maybe Text -> (String -> IO ()) -> IO ()
recoverDiscoveryWithClient client requested report = do
  (owner, objects) <- getManagedObjects client
  adapter <- maybe (fail "No matching powered BlueZ adapter to recover") pure $ findAdapter requested objects
  let power enabled =
        void $
          callBoundedTo
            owner
            client
            adapter
            "org.freedesktop.DBus.Properties"
            "Set"
            [toVariant ("org.bluez.Adapter1" :: String), toVariant ("Powered" :: String), toVariant $ toVariant enabled]
  report $ "BlueZ recovery: power-cycling " <> formatObjectPath adapter <> "; shared BLE connections will reconnect"
  (power False >> threadDelay 1000000) `finally` power True
  report "BlueZ recovery: adapter powered on; retrying discovery"

-- dbus checks sender equality locally: use the unique sender from the BlueZ
-- method reply, since received signals never carry the well-known name.
signalRule :: BusName -> InterfaceName -> MemberName -> DBus.MatchRule
signalRule owner iface member =
  DBus.matchAny
    { DBus.matchSender = Just owner
    , DBus.matchInterface = Just iface
    , DBus.matchMember = Just member
    }

findAdapter :: Maybe Text -> ManagedObjects -> Maybe ObjectPath
findAdapter requested = fmap fst . find matches . Map.toList
  where
    matches (path, interfaces) = case Map.lookup "org.bluez.Adapter1" interfaces of
      Nothing -> False
      Just props ->
        (Map.lookup "Powered" props >>= fromVariant) == Just True
          && maybe True (matchesName path props) requested
    matchesName path props target =
      let fullPath = T.pack $ formatObjectPath path
          adapterAddress = Map.lookup "Address" props >>= fromVariant @Text >>= normalizeDeviceId
       in target == fullPath
            || target == T.takeWhileEnd (/= '/') fullPath
            || maybe False (\wanted -> Just wanted == adapterAddress) (normalizeDeviceId target)

getManagedObjects :: DBus.Client -> IO (BusName, ManagedObjects)
getManagedObjects client = do
  reply <- callBounded client "/" "org.freedesktop.DBus.ObjectManager" "GetManagedObjects" []
  case reply.methodReturnBody of
    [body] | Just objects <- fromVariant body, Just owner <- reply.methodReturnSender -> pure (owner, objects)
    _ -> fail "Invalid BlueZ GetManagedObjects response"

callBounded :: DBus.Client -> ObjectPath -> InterfaceName -> MemberName -> [Variant] -> IO MethodReturn
callBounded = callBoundedTo "org.bluez"

callBoundedTo :: BusName -> DBus.Client -> ObjectPath -> InterfaceName -> MemberName -> [Variant] -> IO MethodReturn
callBoundedTo owner client path iface member body = do
  bounded (formatMemberName member) $
    DBus.call
      client
      (methodCall path iface member)
        { methodCallDestination = Just owner
        , methodCallBody = body
        }
      >>= either (throwIO . BluezCallError) pure

bounded :: String -> IO a -> IO a
bounded label action = do
  reply <- timeout 5000000 action
  maybe (throwIO $ userError $ "BlueZ call timed out: " <> label) pure reply
