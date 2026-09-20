{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Pure decoding of SwitchBot environmental sensor advertisements.
module Network.SwitchBot.Advertisement (
  Advertisement (..),
  SensorModel (..),
  SensorReading (..),
  DecodeError (..),
  decodeAdvertisement,
  normalizeDeviceId,
  switchBotCompanyId,
) where

import Control.Monad (guard)
import Data.Aeson (ToJSON)
import Data.Bits (testBit, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isHexDigit)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16, Word8)
import GHC.Generics (Generic)
import Numeric (showHex)

-- | Payloads exclude the AD length/type, service UUID and company ID bytes.
data Advertisement = Advertisement
  { serviceData :: ![(Text, ByteString)]
  , manufacturerData :: ![(Word16, ByteString)]
  }
  deriving stock (Show, Eq, Generic)

data SensorModel = Hub2 | MeterProCO2 | IndoorOutdoorMeter
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON)

{- | Temperatures are always Celsius, regardless of the device display setting.
Invalid/unavailable individual measurements are 'Nothing'. Light is the
Hub 2's dimensionless level, not a measured lux value.
-}
data SensorReading = SensorReading
  { deviceId :: !Text
  , model :: !SensorModel
  , temperatureC :: !(Maybe Double)
  , humidityPercent :: !(Maybe Int)
  , co2Ppm :: !(Maybe Int)
  , batteryPercent :: !(Maybe Int)
  , lightLevel :: !(Maybe Int)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON)

data DecodeError = TruncatedServiceData | MissingManufacturerData | TruncatedManufacturerData !SensorModel !Int !Int
  deriving stock (Show, Eq, Ord, Generic)

switchBotCompanyId :: Word16
switchBotCompanyId = 0x0969

{- | Accept a six-byte MAC with optional colon/hyphen separators. The canonical
ID comes from the advertisement, including on macOS where CoreBluetooth hides
the peripheral's MAC behind a platform-specific UUID.
-}
normalizeDeviceId :: Text -> Maybe Text
normalizeDeviceId input = do
  let value = T.toUpper $ T.filter (`notElem` (":-" :: String)) input
  guard $ T.length value == 12 && T.all (\c -> c < '\x80' && isHexDigit c) value
  pure value

{- | Unknown devices are ignored. A recognized but incomplete advertisement is
reported separately from an unrelated packet. No pairing or GATT is needed.
-}
decodeAdvertisement :: Advertisement -> Either DecodeError (Maybe SensorReading)
decodeAdvertisement adv = case find (isSensorService . fst) adv.serviceData of
  Nothing -> Right Nothing
  Just (_, service) -> case BS.uncons service of
    Nothing -> Left TruncatedServiceData
    Just (code, _) -> case sensorModel (code .&. 0x7f) of
      Nothing -> Right Nothing
      Just model -> do
        let requiredService = if model == Hub2 then 2 else 3
        if BS.length service < requiredService then Left TruncatedServiceData else pure ()
        manufacturer <- maybe (Left MissingManufacturerData) Right $ lookup switchBotCompanyId adv.manufacturerData
        let required = case model of
              Hub2 -> 16
              MeterProCO2 -> 15
              _ -> 11
        if BS.length manufacturer < required
          then Left $ TruncatedManufacturerData model required (BS.length manufacturer)
          else pure ()
        let at = BS.index manufacturer
            offset = if model == Hub2 then 13 else 8
            fraction = at offset .&. 0x0f
            whole = at (offset + 1) .&. 0x7f
            positive = testBit (at (offset + 1)) 7
            humidity = at (offset + 2) .&. 0x7f
            magnitude = fromIntegral whole + fromIntegral fraction / 10
            temp = if fraction <= 9 && whole /= 127 then Just $ if positive then magnitude else negate magnitude else Nothing
            battery = if model == Hub2 then Nothing else percent (BS.index service 2 .&. 0x7f)
            disconnected = model == Hub2 && temp == Just 0 && humidity == 0
            uninitialized = temp == Just 0 && humidity == 0 && battery == Just 0
            co2 = fromIntegral (at 13) * 256 + fromIntegral (at 14)
        pure $
          Just
            SensorReading
              { deviceId = T.pack $ concatMap hexByte $ BS.unpack $ BS.take 6 manufacturer
              , model
              , temperatureC = if disconnected || uninitialized then Nothing else temp
              , humidityPercent = if disconnected || uninitialized then Nothing else percent humidity
              , co2Ppm = if model == MeterProCO2 && co2 > 0 && co2 <= 9999 then Just co2 else Nothing
              , batteryPercent = battery
              , lightLevel = if model == Hub2 then Just $ fromIntegral (at 12 .&. 0x1f) else Nothing
              }

percent :: Word8 -> Maybe Int
percent n = if n <= 100 then Just (fromIntegral n) else Nothing

hexByte :: Word8 -> String
hexByte b = let s = showHex b "" in map upper $ if b < 16 then '0' : s else s
  where
    upper c = if c >= 'a' && c <= 'f' then toEnum (fromEnum c - 32) else c

isSensorService :: Text -> Bool
isSensorService uuid = T.toLower uuid `elem` ["fd3d", "0000fd3d-0000-1000-8000-00805f9b34fb"]

sensorModel :: Word8 -> Maybe SensorModel
sensorModel code = case code of
  0x76 -> Just Hub2
  0x56 -> Just Hub2
  0x35 -> Just MeterProCO2
  0x15 -> Just MeterProCO2
  0x77 -> Just IndoorOutdoorMeter
  0x57 -> Just IndoorOutdoorMeter
  _ -> Nothing
