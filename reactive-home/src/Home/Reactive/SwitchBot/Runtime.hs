{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Home.Reactive.SwitchBot.Runtime (runSwitchBot, deliverPending) where

import Control.Concurrent qualified as IO
import Control.Exception.Safe qualified as E
import Control.Monad (forM_, forever, unless)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Concurrent.Async (concurrently_, race)
import Effectful.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar)
import FRP.Rhine hiding (forever)
import Home.Reactive.Duration (Duration (..))
import Home.Reactive.Metrics.Hometrics
import Home.Reactive.Orphans ()
import Home.Reactive.Sensor
import Home.Reactive.SwitchBot
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
#ifdef SWITCHBOT_BLUEZ
import Network.SwitchBot.Bluez (scanSensors)
#else
import Network.SwitchBot.SimpleBLE (scanSensors)
#endif
import System.IO (hPutStrLn, stderr)

{- | Run scanning and each enabled delivery worker concurrently. Destination
queues coalesce by configured sensor name, keeping memory bounded during
outages. This runs independently of the application's MQTT input clock.
-}
runSwitchBot ::
  (Concurrent :> es, IOE :> es) =>
  SwitchBotConfig ->
  Maybe HometricsConfig ->
  Maybe ([SensorSample] -> Eff es ()) ->
  (Text -> Eff es ()) ->
  (SwitchBotUpdate -> Eff es ()) ->
  Eff es ()
runSwitchBot cfg hometrics relay report observe = do
  httpPending <- newTVarIO Map.empty
  mqttPending <- newTVarIO Map.empty
  let httpSensors = Map.filter (not . null . hometricsFields) $ hometricsSensorConfigs cfg
      scan =
        E.handleAny
          ( \err -> do
              hPutStrLn stderr $ "SwitchBot scan failed; retrying: " <> show err
              IO.threadDelay (milliseconds * 1000)
              pure []
          )
          $ scanSensors cfg.adapter milliseconds (hPutStrLn stderr . ("SwitchBot advertisement: " <>))
      network =
        ( proc () -> do
            readings <- tagS -< ()
            update <- switchBotS cfg -< readings
            arrMCl observe -< update
            arrMCl
              ( \samples -> atomically $ do
                  let latest = Map.fromList [(s.sensor, s) | s <- samples]
                  forM_ hometrics $ \_ -> modifyTVar' httpPending (Map.union $ Map.intersection latest httpSensors)
                  forM_ relay $ \_ -> modifyTVar' mqttPending (Map.union $ Map.intersection latest $ mqttRelayTopics cfg)
              )
              -<
                update.samples
        )
          @@ SwitchBotClock scan
      deliver label pending send = forever $ do
        -- Sleep in small chunks so large configured intervals do not overflow
        -- the Int microsecond argument on any supported host.
        delaySeconds (reportInterval cfg).seconds
        deliverPending (staleAfter cfg) pending send (report . ((label <> ": ") <>))
      httpWorker = forM_ hometrics $ \hc -> do
        manager <- liftIO $ newManager tlsManagerSettings
        deliver "Hometrics delivery failed; will retry" httpPending (liftIO . postHometrics manager hc httpSensors)
      mqttWorker = forM_ relay $ deliver "SwitchBot MQTT delivery failed; will retry" mqttPending
  flow network `concurrently_` httpWorker `concurrently_` mqttWorker
  where
    milliseconds = round $ (scanWindow cfg).seconds * 1000

{- | Failed deliveries stay pending; successful deliveries acknowledge only the
versions that were actually sent. The callback may run concurrently with BLE.
-}
deliverPending ::
  (Concurrent :> es, IOE :> es) =>
  Duration -> TVar SensorSnapshot -> ([SensorSample] -> Eff es ()) -> (Text -> Eff es ()) -> Eff es ()
deliverPending ttl pending send report = do
  now <- liftIO getCurrentTime
  batch <- atomically $ do
    modifyTVar' pending (freshSamples ttl now)
    readTVar pending
  unless (Map.null batch) $ do
    result <- E.tryAny $ race (threadDelay 10000000) (send $ Map.elems batch)
    case result of
      Left err -> report $ T.pack $ show err
      Right (Left ()) -> report "delivery timed out after 10 seconds"
      Right (Right ()) -> atomically $ modifyTVar' pending (pendingAfterDelivery batch)

delaySeconds :: (Concurrent :> es) => Double -> Eff es ()
delaySeconds remaining
  | remaining <= 0 = pure ()
  | otherwise = do
      let chunk = min 60 remaining
      threadDelay $ max 1 $ round $ chunk * 1000000
      delaySeconds (remaining - chunk)
