{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Home.Reactive.SwitchBot.ScannerTest (test_scanSupervisor) where

import Control.Exception (AsyncException (ThreadKilled), Exception, SomeException, fromException, throwIO, try)
import Control.Monad (forM_, void)
import Data.IORef
import Data.List (isInfixOf)
import Home.Reactive.SwitchBot.Scanner
import Network.SwitchBot.Advertisement (SensorModel (MeterProCO2), SensorReading (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

data Failure = Busy | Denied deriving stock (Show)

instance Exception Failure

data Fixture = Fixture
  { runScan :: IO [SensorReading]
  , at :: Double -> IO ()
  , result :: IORef (IO [SensorReading])
  , recoveries :: IORef Int
  , recoveryFails :: IORef Bool
  , logs :: IORef [String]
  , waits :: IORef Int
  }

fixture :: Bool -> IO Fixture
fixture recoveryEnabled = do
  clock <- newIORef 0
  result <- newIORef $ throwIO Busy
  recoveries <- newIORef 0
  recoveryFails <- newIORef False
  logs <- newIORef []
  waits <- newIORef 0
  let recover err = case fromException @Failure err of
        Just Busy | recoveryEnabled -> Just $ do
          modifyIORef' recoveries (+ 1)
          failed <- readIORef recoveryFails
          if failed then throwIO Denied else pure ()
        _ -> Nothing
  scan <-
    newSupervisedScan
      ScannerActions
        { scan = readIORef result >>= id
        , monotonicSeconds = readIORef clock
        , waitAfterFailure = modifyIORef' waits (+ 1)
        , report = \line -> modifyIORef' logs (<> [line])
        , recovery = recover
        }
  pure $ Fixture scan (writeIORef clock) result recoveries recoveryFails logs waits

step :: Fixture -> Double -> IO ()
step f time = f.at time >> f.runScan >>= (@?= [])

test_scanSupervisor :: TestTree
test_scanSupervisor =
  testGroup
    "BLE scan supervision"
    [ testCase "persistent Busy triggers recovery only after 60 seconds and respects cooldown" $ do
        f <- fixture True
        forM_ [0, 5, 30, 59] $ step f
        readIORef f.recoveries >>= (@?= 0)
        step f 60
        readIORef f.recoveries >>= (@?= 1)
        forM_ [65, 120, 359] $ step f
        readIORef f.recoveries >>= (@?= 1)
        step f 360
        readIORef f.recoveries >>= (@?= 2)
        readIORef f.waits >>= (@?= 9)
    , testCase "quiet but successful scans do not reset the adapter" $ do
        f <- fixture True
        writeIORef f.result $ pure []
        forM_ [0, 60, 600, 3600] $ step f
        readIORef f.recoveries >>= (@?= 0)
        readIORef f.waits >>= (@?= 0)
        readIORef f.logs >>= (@?= [])
    , testCase "successful scan resets the error streak but requires readings to report healthy" $ do
        f <- fixture True
        step f 0
        step f 60
        writeIORef f.result $ pure []
        step f 70
        messages <- readIORef f.logs
        assertBool "empty success does not claim recovery" $ "SwitchBot scanner healthy again" `notElem` messages
        writeIORef f.result $ throwIO Busy
        step f 300
        step f 359
        readIORef f.recoveries >>= (@?= 1)
        step f 360
        readIORef f.recoveries >>= (@?= 2)
        let reading = SensorReading "AABBCCDDEEFF" MeterProCO2 (Just 25.3) (Just 66) (Just 584) (Just 100) Nothing
        writeIORef f.result $ pure [reading]
        f.at 365
        f.runScan >>= (@?= [reading])
        f.runScan >>= (@?= [reading])
        recovered <- readIORef f.logs
        length (filter (== "SwitchBot scanner healthy again") recovered) @?= 1
    , testCase "failed recovery is rate-limited too" $ do
        f <- fixture True
        writeIORef f.recoveryFails True
        forM_ [0, 60, 65, 120, 359] $ step f
        readIORef f.recoveries >>= (@?= 1)
        messages <- readIORef f.logs
        assertBool "recovery failure logged" $ any (isInfixOf "recovery failed") messages
    , testCase "permission error breaks a recoverable failure streak" $ do
        f <- fixture True
        step f 0
        writeIORef f.result $ throwIO Denied
        step f 59
        writeIORef f.result $ throwIO Busy
        forM_ [60, 119] $ step f
        readIORef f.recoveries >>= (@?= 0)
        step f 120
        readIORef f.recoveries >>= (@?= 1)
    , testCase "disabled recovery and SimpleBLE still retry without resetting the adapter" $ do
        f <- fixture False
        forM_ [0, 60, 600] $ step f
        readIORef f.recoveries >>= (@?= 0)
        readIORef f.waits >>= (@?= 3)
    , testCase "cancellation escapes instead of becoming an empty scan" $ do
        f <- fixture True
        writeIORef f.result $ throwIO ThreadKilled
        result <- try @SomeException $ void f.runScan
        case result of
          Right () -> assertFailure "cancellation swallowed"
          Left err -> fromException @AsyncException err @?= Just ThreadKilled
        readIORef f.waits >>= (@?= 0)
        readIORef f.recoveries >>= (@?= 0)
    ]
