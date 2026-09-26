module Network.SwitchBot.Bluez.ScannerTest (test_persistentScanner) where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket, throwIO, try)
import Control.Monad (unless, void, when)
import DBus
import DBus.Client qualified as DBus
import Data.Map.Strict qualified as Map
import Network.SwitchBot.Advertisement
import Network.SwitchBot.Bluez
import Network.SwitchBot.Bluez.Advertisement (ManagedObjects)
import Network.SwitchBot.BluezTest (adapter, added, changed, device, objects, properties)
import System.IO (hGetLine)
import System.Process
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

data Mock = Mock
  { server :: DBus.Client
  , client :: DBus.Client
  , address :: Address
  , starts :: TVar Int
  , stops :: TVar Int
  , discovering :: TVar Bool
  , powered :: TVar Bool
  , powerCalls :: TVar [Bool]
  , failure :: TVar (Maybe ErrorName)
  , emitReadings :: TVar Bool
  , failPowerOff :: TVar Bool
  }

withMock :: (Mock -> IO ()) -> IO ()
withMock use = do
  done <- timeout 20000000
    $ withCreateProcess
      (proc "dbus-daemon" ["--session", "--address=unix:tmpdir=/tmp", "--nofork", "--print-address=1"]) {std_out = CreatePipe}
    $ \_ output _ _ -> do
      busAddress <- maybe (fail "Missing bus") hGetLine output >>= maybe (fail "Invalid bus") pure . parseAddress
      bracket (DBus.connect busAddress) DBus.disconnect $ \server ->
        bracket (DBus.connect busAddress) DBus.disconnect $ \client -> do
          mock <-
            Mock server client busAddress
              <$> newTVarIO 0
              <*> newTVarIO 0
              <*> newTVarIO False
              <*> newTVarIO True
              <*> newTVarIO []
              <*> newTVarIO Nothing
              <*> newTVarIO True
              <*> newTVarIO False
          exportMock server mock
          use mock
  unless (done == Just ()) $ assertFailure "private D-Bus test timed out"

exportMock :: DBus.Client -> Mock -> IO ()
exportMock server mock = do
  _ <- DBus.requestName server "org.bluez" [DBus.nameDoNotQueue]
  let managed :: IO ManagedObjects
      managed = do
        powered <- readTVarIO mock.powered
        pure $ Map.adjust (Map.adjust (Map.insert "Powered" $ toVariant powered) "org.bluez.Adapter1") adapter objects
      start = do
        atomically $ modifyTVar' mock.starts (+ 1)
        failure <- readTVarIO mock.failure
        case failure of
          Just name -> DBus.throwError name "discovery failed" []
          Nothing -> do
            atomically $ writeTVar mock.discovering True
            emitReadings <- readTVarIO mock.emitReadings
            when emitReadings $ DBus.emit server $ added MeterProCO2
      stop = atomically $ do
        modifyTVar' mock.stops (+ 1)
        writeTVar mock.discovering False
      power value = do
        atomically $ do
          modifyTVar' mock.powerCalls (<> [value])
          writeTVar mock.powered value
          writeTVar mock.discovering False
          when value $ writeTVar mock.failure Nothing
        failOff <- readTVarIO mock.failPowerOff
        when (failOff && not value) $ DBus.throwError "org.bluez.Error.Failed" "power-off reply lost" []
  DBus.export
    server
    "/"
    DBus.defaultInterface
      { DBus.interfaceName = "org.freedesktop.DBus.ObjectManager"
      , DBus.interfaceMethods = [DBus.autoMethod "GetManagedObjects" managed]
      }
  DBus.export
    server
    adapter
    DBus.defaultInterface
      { DBus.interfaceName = "org.bluez.Adapter1"
      , DBus.interfaceMethods =
          [ DBus.autoMethod "StartDiscovery" start
          , DBus.autoMethod "StopDiscovery" stop
          , DBus.autoMethod "SetDiscoveryFilter" (\(_ :: Map.Map String Variant) -> pure () :: IO ())
          ]
      , DBus.interfaceProperties =
          [ DBus.autoProperty "Powered" (Just $ readTVarIO mock.powered) (Just power)
          , DBus.readOnlyProperty "Discovering" (readTVarIO mock.discovering)
          ]
      }

expectFailure :: Bool -> IO a -> IO ()
expectFailure recoverable action = do
  result <- try @SomeException action
  case result of
    Right _ -> assertFailure "expected a scan failure"
    Left err -> isDiscoveryFailure err @?= recoverable

test_persistentScanner :: TestTree
test_persistentScanner =
  testGroup
    "persistent BlueZ scanner"
    [ testCase "one discovery session spans windows without replaying cached readings" $ withMock $ \mock -> do
        withScannerWithClient mock.client Nothing assertFailure $ \scan -> do
          scan 50 >>= \readings -> map (.co2Ppm) readings @?= [Just 584]
          scan 20 >>= (@?= [])
          DBus.emit mock.server $ changed device (Map.delete "ManufacturerData" $ properties MeterProCO2) []
          scan 20 >>= (@?= [])
          DBus.emit mock.server $ changed device (Map.delete "ServiceData" $ properties MeterProCO2) []
          scan 50 >>= \readings -> map (.co2Ppm) readings @?= [Just 584]
          readTVarIO mock.starts >>= (@?= 1)
          readTVarIO mock.stops >>= (@?= 0)
        readTVarIO mock.stops >>= (@?= 1)
    , testCase "lost discovery is detected and starts a fresh session" $ withMock $ \mock ->
        withScannerWithClient mock.client Nothing assertFailure $ \scan -> do
          void $ scan 20
          atomically $ writeTVar mock.discovering False
          expectFailure True $ scan 20
          atomically $ writeTVar mock.emitReadings False
          scan 20 >>= (@?= [])
          readTVarIO mock.starts >>= (@?= 2)
    , testCase "Busy survives reconnect but adapter recovery clears it" $ withMock $ \mock ->
        withScannerWithClient mock.client Nothing assertFailure $ \scan -> do
          atomically $ writeTVar mock.failure $ Just "org.bluez.Error.InProgress"
          expectFailure True $ scan 20
          expectFailure True $ scan 20
          recoverDiscoveryWithClient mock.client (Just "hci0") (const $ pure ())
          readTVarIO mock.powerCalls >>= (@?= [False, True])
          scan 50 >>= \readings -> map (.co2Ppm) readings @?= [Just 584]
    , testCase "authorization failures never qualify for adapter recovery" $ withMock $ \mock -> do
        atomically $ writeTVar mock.failure $ Just "org.bluez.Error.NotAuthorized"
        expectFailure False $ scanSensorsWithClient mock.client Nothing 20 assertFailure
    , testCase "manually powered-off adapter is not switched on by recovery" $ withMock $ \mock -> do
        atomically $ writeTVar mock.powered False
        expectFailure False $ recoverDiscoveryWithClient mock.client Nothing (const $ pure ())
        readTVarIO mock.powerCalls >>= (@?= [])
    , testCase "power-on is attempted even when power-off reports failure" $ withMock $ \mock -> do
        atomically $ writeTVar mock.failPowerOff True
        expectFailure False $ recoverDiscoveryWithClient mock.client Nothing (const $ pure ())
        readTVarIO mock.powerCalls >>= (@?= [False, True])
        readTVarIO mock.powered >>= (@?= True)
    , testCase "cancelling recovery still restores adapter power" $ withMock $ \mock -> do
        done <- newEmptyMVar
        tid <- forkIO $ try @SomeException (recoverDiscoveryWithClient mock.client Nothing (const $ pure ())) >>= putMVar done
        atomically $ readTVar mock.powered >>= check . not
        killThread tid
        result <- takeMVar done
        either (const $ pure ()) (const $ assertFailure "recovery was not cancelled") result
        readTVarIO mock.powerCalls >>= (@?= [False, True])
        readTVarIO mock.powered >>= (@?= True)
    , testCase "daemon replacement rebinds signals and discards old readings" $ withMock $ \mock ->
        withScannerWithClient mock.client Nothing (const $ pure ()) $ \scan -> do
          void $ scan 20
          void $ DBus.releaseName mock.server "org.bluez"
          DBus.disconnect mock.server
          bracket (DBus.connect mock.address) DBus.disconnect $ \replacement -> do
            exportMock replacement mock
            expectFailure False $ scan 20
            atomically $ writeTVar mock.emitReadings False
            scan 20 >>= (@?= [])
            DBus.emit replacement $ added Hub2
            scan 50 >>= \readings -> map (.model) readings @?= [Hub2]
    , testCase "cancellation releases the persistent discovery session" $ withMock $ \mock -> do
        done <- newEmptyMVar
        tid <- forkIO $ try @SomeException (withScannerWithClient mock.client Nothing assertFailure $ \scan -> void $ scan 10000) >>= putMVar done
        atomically $ readTVar mock.discovering >>= check
        killThread tid
        result <- takeMVar done
        either (const $ pure ()) (const $ assertFailure "scan was not cancelled") result
        readTVarIO mock.stops >>= (@?= 1)
    , testCase "exception in consumer still releases discovery" $ withMock $ \mock -> do
        expectFailure False $ withScannerWithClient mock.client Nothing assertFailure $ \scan -> do
          void $ scan 20
          throwIO $ userError "consumer failed"
        readTVarIO mock.stops >>= (@?= 1)
    ]
