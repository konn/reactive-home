-- | Advertisement-only scanning through the Linux BlueZ D-Bus API.
module Network.SwitchBot.Bluez (scanSensors, scanSensorsWithClient) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception.Safe (bracket, bracket_, onException, throwIO, tryAny)
import Control.Monad (unless, void)
import DBus
import DBus.Client qualified as DBus
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Network.SwitchBot.Advertisement (SensorReading, normalizeDeviceId)
import Network.SwitchBot.Bluez.Advertisement
import System.Timeout (timeout)

-- | A new system-bus connection owns and releases each finite discovery session.
scanSensors :: Maybe Text -> Int -> (String -> IO ()) -> IO [SensorReading]
scanSensors requested milliseconds report =
  bracket DBus.connectSystem DBus.disconnect $ \client -> scanSensorsWithClient client requested milliseconds report

{- | Injectable D-Bus client for tests or a caller-owned dedicated connection.
Do not reuse a connection that already owns another discovery session.
-}
scanSensorsWithClient :: DBus.Client -> Maybe Text -> Int -> (String -> IO ()) -> IO [SensorReading]
scanSensorsWithClient client requested milliseconds report = do
  unless (milliseconds > 0 && milliseconds <= 60000) $ fail "Scan window must be between 1 and 60000 milliseconds"
  (owner, objects) <- getManagedObjects client
  adapter <- maybe (fail "No matching powered BlueZ adapter") pure $ findAdapter requested objects
  state <- newTVarIO $ initialScan adapter objects
  let receive event = atomically $ modifyTVar' state $ collectSignal adapter event
      matched rule =
        bracket
          (bounded "AddMatch" $ DBus.addMatch client rule receive)
          (bounded "RemoveMatch" . DBus.removeMatch client)
          . const
      adapterCall member body = void $ callBounded client adapter "org.bluez.Adapter1" member body
      stop = tryAny (adapterCall "StopDiscovery" []) >>= either (report . show) pure
      filterOptions =
        Map.fromList
          [ ("Transport", toVariant ("le" :: String))
          , ("DuplicateData", toVariant True)
          ] ::
          Map.Map String Variant
  matched (signalRule owner "org.freedesktop.DBus.Properties" "PropertiesChanged") $
    matched (signalRule owner "org.freedesktop.DBus.ObjectManager" "InterfacesAdded") $
      matched (signalRule owner "org.freedesktop.DBus.ObjectManager" "InterfacesRemoved") $ do
        adapterCall "SetDiscoveryFilter" [toVariant filterOptions]
        result <- bracket_ (adapterCall "StartDiscovery" [] `onException` stop) stop $ do
          threadDelay $ milliseconds * 1000
          readTVarIO state
        mapM_ (report . show) $ scanErrors result
        pure $ scanResults result

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
callBounded client path iface member body = do
  bounded (formatMemberName member) $
    DBus.call_
      client
      (methodCall path iface member)
        { methodCallDestination = Just "org.bluez"
        , methodCallBody = body
        }

bounded :: String -> IO a -> IO a
bounded label action = do
  reply <- timeout 5000000 action
  maybe (throwIO $ userError $ "BlueZ call timed out: " <> label) pure reply
