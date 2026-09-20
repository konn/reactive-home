{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Advertisement-only scanning using the same SimpleBLE binding as Sesame.
module Network.SwitchBot.SimpleBLE (getScanner, scanReadings, scanSensors) where

import Control.Exception.Safe (throwIO, tryAny)
import Control.Monad (forM, unless)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Network.SwitchBot.Advertisement
import SimpleBLE qualified as BLE

-- | Same finite-window entry point as the BlueZ backend.
scanSensors :: Maybe Text -> Int -> (String -> IO ()) -> IO [SensorReading]
scanSensors requested milliseconds report = do
  adapter <- getScanner requested
  scanReadings adapter milliseconds report

-- | Select the first adapter by default, or match its identifier/address.
getScanner :: Maybe Text -> IO BLE.Adapter
getScanner requested = do
  enabled <- BLE.bluetoothEnabled
  unless enabled $ throwIO $ BLE.SimpleBLEException "Bluetooth is not enabled or is not accessible"
  adapters <- BLE.getAdapters
  candidates <- case requested of
    Nothing -> pure adapters
    Just target -> fmap catMaybes $ forM adapters $ \adapter -> do
      identifier <- BLE.adapterIdentifier adapter
      address <- BLE.adapterAddress adapter
      pure $ if target == identifier || target == address then Just adapter else Nothing
  case candidates of
    adapter : _ -> pure adapter
    [] -> throwIO $ BLE.SimpleBLEException "No matching Bluetooth adapter"

{- | Each finite window returns the latest advertisement from each peripheral
seen in that window. SimpleBLE clears scan results when starting a new scan.
Device-level errors are reported without aborting the other devices. Adapter
errors propagate to the supervisor. This function never connects to a device.
-}
scanReadings :: BLE.Adapter -> Int -> (String -> IO ()) -> IO [SensorReading]
scanReadings adapter milliseconds report = do
  unless (milliseconds > 0 && milliseconds <= 60000) $
    throwIO $
      BLE.SimpleBLEException "Scan window must be between 1 and 60000 milliseconds"
  BLE.adapterScanFor adapter milliseconds
  peripherals <- BLE.adapterScanGetResults adapter
  fmap catMaybes $ forM peripherals $ \peripheral -> do
    result <- tryAny $ do
      manufacturers <- BLE.peripheralManufacturerData peripheral
      if not $ any ((== switchBotCompanyId) . (.manufacturerId)) manufacturers
        then pure $ Right Nothing
        else do
          services <- BLE.peripheralServices peripheral
          pure $
            decodeAdvertisement
              Advertisement
                { serviceData = [(s.uuid, s.data_) | s <- services]
                , manufacturerData = [(m.manufacturerId, m.payload) | m <- manufacturers]
                }
    case result of
      Left err -> report (show err) >> pure Nothing
      Right (Left err) -> report (show err) >> pure Nothing
      Right (Right reading) -> pure reading
