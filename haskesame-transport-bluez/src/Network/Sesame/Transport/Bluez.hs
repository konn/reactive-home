module Network.Sesame.Transport.Bluez (
  BluezConfig (..),
  defaultBluezConfig,
  connectBluez,
) where

import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import DBus
import DBus.Client qualified as DBus
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word8)
import Network.Sesame.Codec
import Network.Sesame.Exception
import Network.Sesame.Transport

data BluezConfig = BluezConfig
  { devicePath :: !ObjectPath
  , writeCharacteristicPath :: !ObjectPath
  , notifyCharacteristicPath :: !ObjectPath
  , manufacturerData :: !(Maybe ByteString)
  }
  deriving stock (Show, Eq)

defaultBluezConfig :: ObjectPath -> ObjectPath -> ObjectPath -> BluezConfig
defaultBluezConfig device writeChar notifyChar =
  BluezConfig
    { devicePath = device
    , writeCharacteristicPath = writeChar
    , notifyCharacteristicPath = notifyChar
    , manufacturerData = Nothing
    }

connectBluez :: BluezConfig -> IO (Either SesameTransportError SesameTransport)
connectBluez config = do
  result <- try do
    client <- DBus.connectSystem
    queue <- newTQueueIO
    state <- newTVarIO emptyReassembly
    _ <- DBus.addMatch client (propertiesChangedRule config.notifyCharacteristicPath) (handleSignal queue state)
    callNoBody client config.devicePath "org.bluez.Device1" "Connect"
    callNoBody client config.notifyCharacteristicPath "org.bluez.GattCharacteristic1" "StartNotify"
    pure
      SesameTransport
        { sendBle = \encrypted bytes -> sendFragments client config.writeCharacteristicPath encrypted bytes
        , receiveBle = atomically (readTQueue queue)
        , closeBle = do
            _ <- try @SomeException (callNoBody client config.notifyCharacteristicPath "org.bluez.GattCharacteristic1" "StopNotify")
            _ <- try @SomeException (callNoBody client config.devicePath "org.bluez.Device1" "Disconnect")
            DBus.disconnect client
        , advertisement = pure (maybe (Left AdvertisementUnavailable) (either (Left . TransportCallFailed . show) Right . decodeAdvertisement) config.manufacturerData)
        }
  pure (either (Left . TransportCallFailed . show @SomeException) Right result)

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
