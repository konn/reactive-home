module Network.SwitchBot.BluezTest (test_signalCollection, test_dbusDiscovery, adapter, device, objects, properties, changed, added) where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket, try)
import Control.Monad (unless, void, when, (>=>))
import DBus
import DBus.Client qualified as DBus
import Data.ByteString qualified as BS
import Data.Int (Int16)
import Data.Map.Strict qualified as Map
import Data.Word (Word16)
import Network.SwitchBot.Advertisement
import Network.SwitchBot.Bluez
import Network.SwitchBot.Bluez.Advertisement
import System.IO (hGetLine)
import System.Process
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

adapter, device :: ObjectPath
adapter = "/org/bluez/hci0"
device = "/org/bluez/hci0/dev_AA_BB_CC_DD_EE_FF"

-- Same captured model/payload bytes as switchbot-core's fixtures.
properties :: SensorModel -> BluezProperties
properties model =
  Map.fromList
    [ ("ServiceData", toVariant $ Map.singleton ("0000fd3d-0000-1000-8000-00805f9b34fb" :: String) $ toVariant $ BS.pack service)
    , ("ManufacturerData", toVariant $ Map.singleton (0x0969 :: Word16) $ toVariant $ BS.pack $ [0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff] <> payload)
    ]
  where
    (service, payload) = case model of
      Hub2 -> ([118, 0], [0, 255, 106, 175, 249, 44, 77, 0, 153, 73, 0])
      MeterProCO2 -> ([53, 0, 100], [46, 228, 3, 153, 66, 0, 41, 2, 72, 0])
      IndoorOutdoorMeter -> ([119, 128, 79], [200, 10, 8, 149, 224, 0])

objects :: ManagedObjects
objects =
  Map.fromList
    [ (adapter, Map.singleton "org.bluez.Adapter1" $ Map.fromList [("Powered", toVariant True), ("Address", toVariant ("11:22:33:44:55:66" :: String))])
    , (device, Map.singleton "org.bluez.Device1" $ properties MeterProCO2)
    ]

changed :: ObjectPath -> BluezProperties -> [String] -> Signal
changed path props removed =
  (signal path "org.freedesktop.DBus.Properties" "PropertiesChanged")
    { signalBody = [toVariant ("org.bluez.Device1" :: String), toVariant props, toVariant removed]
    }

added :: SensorModel -> Signal
added model =
  (signal "/" "org.freedesktop.DBus.ObjectManager" "InterfacesAdded")
    { signalBody = [toVariant device, toVariant $ Map.singleton ("org.bluez.Device1" :: String) $ properties model]
    }

test_signalCollection :: TestTree
test_signalCollection =
  testGroup
    "BlueZ advertisements"
    [ testCase "cached properties do not count as a fresh scan" $
        scanResults (initialScan adapter objects) @?= []
    , testGroup
        "all captured sensor models"
        [ testCase (show model) $ do
            let state = collectSignal adapter (added model) $ initialScan adapter Map.empty
            map (.model) (scanResults state) @?= [model]
            map (.deviceId) (scanResults state) @?= ["AABBCCDDEEFF"]
            scanErrors state @?= []
        | model <- [Hub2, MeterProCO2, IndoorOutdoorMeter]
        ]
    , testCase "fresh manufacturer data combines with cached service model" $ do
        let props = Map.filterWithKey (\key _ -> key == "ManufacturerData") $ properties MeterProCO2
            state = collectSignal adapter (changed device props []) $ initialScan adapter objects
        map (.co2Ppm) (scanResults state) @?= [Just 584]
    , testCase "RSSI or service-only changes never refresh stale manufacturer measurements" $ do
        let props = Map.delete "ManufacturerData" $ properties MeterProCO2
            state = collectSignal adapter (changed device (Map.insert "RSSI" (toVariant (-50 :: Int16)) props) []) $ initialScan adapter objects
        scanResults state @?= []
    , testCase "manufacturer before service data is recovered within the window" $ do
        let props = properties MeterProCO2
            first = collectSignal adapter (changed device (Map.delete "ServiceData" props) []) $ initialScan adapter Map.empty
            second = collectSignal adapter (changed device (Map.delete "ManufacturerData" props) []) first
        scanResults first @?= []
        map (.co2Ppm) (scanResults second) @?= [Just 584]
    , testCase "invalidated data and removed devices discard pending readings" $ do
        let state = collectSignal adapter (added MeterProCO2) $ initialScan adapter Map.empty
            invalid = collectSignal adapter (changed device Map.empty ["ManufacturerData"]) state
            removed =
              (signal "/" "org.freedesktop.DBus.ObjectManager" "InterfacesRemoved")
                { signalBody = [toVariant device, toVariant ["org.bluez.Device1" :: String]]
                }
        scanResults invalid @?= []
        scanResults (collectSignal adapter removed state) @?= []
    , testCase "other adapters and malformed signals are ignored" $ do
        let state = initialScan adapter Map.empty
        scanResults (collectSignal adapter (changed "/org/bluez/hci1/dev_AA" (properties MeterProCO2) []) state) @?= []
        scanResults (collectSignal adapter ((added Hub2) {signalBody = []}) state) @?= []
    , testCase "truncated advertisements are observable without crashing" $ do
        let props = Map.insert "ManufacturerData" (toVariant $ Map.singleton (0x0969 :: Word16) $ toVariant $ BS.pack [1, 2]) $ properties MeterProCO2
            state = collectSignal adapter (changed device props []) $ initialScan adapter Map.empty
        scanResults state @?= []
        assertBool "decode error retained" $ not $ null $ scanErrors state
    ]

test_dbusDiscovery :: TestTree
test_dbusDiscovery = testCase "private D-Bus discovery, filtering, freshness and cancellation cleanup" $ do
  completed <- timeout 10000000
    $ withCreateProcess
      (proc "dbus-daemon" ["--session", "--address=unix:tmpdir=/tmp", "--nofork", "--print-address=1"]) {std_out = CreatePipe}
    $ \_ output _ _ -> do
      bus <- maybe (fail "Missing daemon output") hGetLine output
      busAddress <- maybe (fail "Invalid private bus address") pure $ parseAddress bus
      bracket (DBus.connect busAddress) DBus.disconnect $ \server ->
        bracket (DBus.connect busAddress) DBus.disconnect $ \client -> do
          _ <- DBus.requestName server "org.bluez" [DBus.nameDoNotQueue]
          filters <- newTVarIO ([] :: [Map.Map String Variant])
          starts <- newTQueueIO
          stops <- newTVarIO (0 :: Int)
          emitReadings <- newTVarIO True
          let start = do
                atomically $ writeTQueue starts ()
                enabled <- readTVarIO emitReadings
                when enabled $ DBus.emit server $ added MeterProCO2
              stop = atomically $ modifyTVar' stops (+ 1)
              setFilter values = atomically $ modifyTVar' filters (<> [values])
          DBus.export
            server
            "/"
            DBus.defaultInterface
              { DBus.interfaceName = "org.freedesktop.DBus.ObjectManager"
              , DBus.interfaceMethods = [DBus.autoMethod "GetManagedObjects" (pure objects :: IO ManagedObjects)]
              }
          DBus.export
            server
            adapter
            DBus.defaultInterface
              { DBus.interfaceName = "org.bluez.Adapter1"
              , DBus.interfaceProperties =
                  [ DBus.readOnlyProperty "Powered" (pure True)
                  , DBus.readOnlyProperty "Discovering" (pure True)
                  ]
              , DBus.interfaceMethods =
                  [ DBus.autoMethod "SetDiscoveryFilter" setFilter
                  , DBus.autoMethod "StartDiscovery" start
                  , DBus.autoMethod "StopDiscovery" stop
                  ]
              }
          readings <- scanSensorsWithClient client (Just "hci0") 100 assertFailure
          map (.co2Ppm) readings @?= [Just 584]
          atomically $ writeTVar emitReadings False
          stale <- scanSensorsWithClient client (Just "11:22:33:44:55:66") 20 assertFailure
          stale @?= []
          _ <- scanSensorsWithClient client (Just "/org/bluez/hci0") 20 assertFailure
          selected <- readTVarIO filters
          length selected @?= 3
          map (Map.lookup "Transport" >=> fromVariant @String) selected @?= replicate 3 (Just "le")
          map (Map.lookup "DuplicateData" >=> fromVariant @Bool) selected @?= replicate 3 (Just True)
          readTVarIO stops >>= (@?= 3)
          atomically $ void $ flushTQueue starts
          done <- newEmptyMVar
          tid <- forkIO $ try @SomeException (scanSensorsWithClient client Nothing 10000 assertFailure) >>= putMVar done
          atomically $ readTQueue starts
          killThread tid
          result <- takeMVar done
          assertBool "scan cancelled" $ either (const True) (const False) result
          readTVarIO stops >>= (@?= 4)
  unless (completed == Just ()) $ assertFailure "D-Bus test timed out"
