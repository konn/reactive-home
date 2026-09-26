{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Supervise either BLE backend without treating radio silence as a fault.
module Home.Reactive.SwitchBot.Scanner (ScannerActions (..), newSupervisedScan) where

import Control.Exception.Safe (SomeException, tryAny)
import Control.Monad (forM_, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Network.SwitchBot.Advertisement (SensorReading)

data ScannerActions = ScannerActions
  { scan :: IO [SensorReading]
  , monotonicSeconds :: IO Double
  , waitAfterFailure :: IO ()
  , report :: String -> IO ()
  , recovery :: SomeException -> Maybe (IO ())
  }

{- | Single-consumer supervisor. Recovery requires at least 60 seconds of
consecutive recoverable errors and is attempted at most once per five minutes,
even when recovery fails or discovery briefly succeeds. Every failed scan
yields an empty tick so the reactive snapshot continues to expire normally.
-}
newSupervisedScan :: ScannerActions -> IO (IO [SensorReading])
newSupervisedScan actions = do
  state <- newIORef (False, Nothing, Nothing, 0)
  pure $ do
    result <- tryAny actions.scan
    now <- actions.monotonicSeconds
    (wasFailing, since, lastRecovery, lastLog) <- readIORef state
    case result of
      Right readings -> do
        -- An empty successful call is not evidence that measurements resumed.
        -- Clear the error streak, but keep health degraded until a reading arrives.
        let stillFailing = wasFailing && null readings
        when (wasFailing && not stillFailing) $ actions.report "SwitchBot scanner healthy again"
        writeIORef state (stillFailing, Nothing, lastRecovery, if stillFailing then lastLog else now)
        pure readings
      Left err -> do
        let logFailure = not wasFailing || now - lastLog >= 60
        when logFailure $ actions.report $ "SwitchBot scanner unhealthy; retrying: " <> show err
        let recover = actions.recovery err
            started = case recover of
              Nothing -> Nothing
              Just _ -> Just $ fromMaybe now since
            due =
              maybe False (\at -> now - at >= 60) started
                && maybe True (\at -> now - at >= 300) lastRecovery
        -- Record the attempt before running recovery, including failed attempts.
        writeIORef state (True, started, if due then Just now else lastRecovery, if logFailure then now else lastLog)
        when due $ forM_ recover $ \reset -> do
          actions.report "SwitchBot scanner recovery: discovery has failed for at least 60 seconds"
          resetResult <- tryAny reset
          case resetResult of
            Left failure -> actions.report $ "SwitchBot scanner recovery failed (next attempt in at least 5 minutes): " <> show failure
            Right () -> pure ()
        actions.waitAfterFailure
        pure []
