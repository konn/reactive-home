{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.SwitchBot.AdvertisementTest (test_advertisements) where

import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Word (Word8)
import Network.SwitchBot.Advertisement
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

-- Captured through SimpleBLE on macOS, 2026-09-21. Only the first six
-- manufacturer bytes (device MAC) are anonymized; all sensor bytes are intact.
fixture :: SensorModel -> Advertisement
fixture model =
  Advertisement
    [("0000fd3d-0000-1000-8000-00805f9b34fb", BS.pack service)]
    [(0x0969, BS.pack $ [0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff] <> payload)]
  where
    (service, payload) = case model of
      Hub2 -> ([118, 0], [0, 255, 106, 175, 249, 44, 77, 0, 153, 73, 0])
      MeterProCO2 -> ([53, 0, 100], [46, 228, 3, 153, 66, 0, 41, 2, 72, 0])
      _ -> ([119, 128, 79], [200, 10, 8, 149, 224, 0])

expected :: SensorModel -> SensorReading
expected Hub2 = SensorReading "AABBCCDDEEFF" Hub2 (Just 25) (Just 73) Nothing Nothing (Just 13)
expected MeterProCO2 = SensorReading "AABBCCDDEEFF" MeterProCO2 (Just 25.3) (Just 66) (Just 584) (Just 100) Nothing
expected model = SensorReading "AABBCCDDEEFF" model (Just 21.8) (Just 96) Nothing (Just 79) Nothing

test_advertisements :: TestTree
test_advertisements =
  testGroup
    "SwitchBot sensor advertisements"
    [ testGroup
        "captured packets"
        [ testCase (show model) $ decodeAdvertisement (fixture model) @?= Right (Just $ expected model)
        | model <- [Hub2, MeterProCO2, IndoorOutdoorMeter]
        ]
    , testGroup
        "all truncated manufacturer prefixes"
        [ testCase (show (model, size)) $
            assertBool "must report truncation" $
              isLeft $
                decodeAdvertisement $
                  mapManufacturer (BS.take size) $
                    fixture model
        | (model, minimumLength) <- [(Hub2, 16), (MeterProCO2, 15), (IndoorOutdoorMeter, 11)]
        , size <- [0 .. minimumLength - 1]
        ]
    , testGroup
        "truncated service data"
        [ testCase (show (model, size)) $
            assertBool "must report truncation" $
              isLeft $
                decodeAdvertisement $
                  mapService (BS.take size) $
                    fixture model
        | (model, minimumLength) <- [(Hub2, 2), (MeterProCO2, 3), (IndoorOutdoorMeter, 3)]
        , size <- [0 .. minimumLength - 1]
        ]
    , testCase "negative temperatures and Fahrenheit display still decode Celsius" $
        decodeAdvertisement (mapManufacturer (setByte 9 0x15) $ fixture IndoorOutdoorMeter)
          @?= Right (Just $ (expected IndoorOutdoorMeter) {temperatureC = Just (-21.8)})
    , testCase "high status bits do not become temperature decimals" $
        decodeAdvertisement (mapManufacturer (setByte 8 0xf8) $ fixture IndoorOutdoorMeter)
          @?= Right (Just $ expected IndoorOutdoorMeter)
    , testCase "invalid decimal is absent, preserving other readings" $
        decodeAdvertisement (mapManufacturer (setByte 8 0x0f) $ fixture IndoorOutdoorMeter)
          @?= Right (Just $ (expected IndoorOutdoorMeter) {temperatureC = Nothing})
    , testCase "humidity sentinel is absent" $
        decodeAdvertisement (mapManufacturer (setByte 10 0xff) $ fixture IndoorOutdoorMeter)
          @?= Right (Just $ (expected IndoorOutdoorMeter) {humidityPercent = Nothing})
    , testCase "CO2 sentinel is absent, preserving temperature and humidity" $
        decodeAdvertisement (mapManufacturer (setByte 13 0xff . setByte 14 0xff) $ fixture MeterProCO2)
          @?= Right (Just $ (expected MeterProCO2) {co2Ppm = Nothing})
    , testCase "zero CO2 during warmup is absent" $
        decodeAdvertisement (mapManufacturer (setByte 13 0 . setByte 14 0) $ fixture MeterProCO2)
          @?= Right (Just $ (expected MeterProCO2) {co2Ppm = Nothing})
    , testCase "missing Hub 2 probe does not become zero readings" $
        decodeAdvertisement (mapManufacturer (setByte 13 0 . setByte 14 0 . setByte 15 0) $ fixture Hub2)
          @?= Right (Just $ (expected Hub2) {temperatureC = Nothing, humidityPercent = Nothing})
    , testCase "reserved high model bit and shortened uppercase UUID are accepted" $
        decodeAdvertisement
          ( (mapService (setByte 0 0xf7) $ fixture IndoorOutdoorMeter)
              { serviceData = [("FD3D", BS.pack [0xf7, 128, 79])]
              }
          )
          @?= Right (Just $ expected IndoorOutdoorMeter)
    , testCase "unknown SwitchBot product is ignored" $
        decodeAdvertisement (mapService (setByte 0 0x6a) $ fixture Hub2) @?= Right Nothing
    , testCase "unrelated UUID is ignored even with SwitchBot-shaped bytes" $
        decodeAdvertisement ((fixture Hub2) {serviceData = [("abcd", BS.pack [118, 0])]}) @?= Right Nothing
    , testCase "another company cannot be decoded as SwitchBot" $
        decodeAdvertisement ((fixture Hub2) {manufacturerData = [(0x004c, BS.replicate 17 0)]}) @?= Left MissingManufacturerData
    , testCase "empty advertisement is ignored" $ decodeAdvertisement (Advertisement [] []) @?= Right Nothing
    , testCase "future trailing bytes are tolerated" $
        decodeAdvertisement (mapManufacturer (<> BS.pack [0, 1, 2]) $ fixture Hub2) @?= Right (Just $ expected Hub2)
    , testCase "MAC normalization is independent of host Bluetooth IDs" $ do
        normalizeDeviceId "aa:bb:cc:dd:ee:ff" @?= Just "AABBCCDDEEFF"
        normalizeDeviceId "AA-BB-CC-DD-EE-FF" @?= Just "AABBCCDDEEFF"
        map normalizeDeviceId (["", "AABB", "GG1122334455", "718FA46A-6DDD-7CF7-BD2B-11710C6917D6"] :: [Text]) @?= replicate 4 Nothing
    ]

mapManufacturer :: (BS.ByteString -> BS.ByteString) -> Advertisement -> Advertisement
mapManufacturer f adv = adv {manufacturerData = [(key, f value) | (key, value) <- adv.manufacturerData]}

mapService :: (BS.ByteString -> BS.ByteString) -> Advertisement -> Advertisement
mapService f adv = adv {serviceData = [(key, f value) | (key, value) <- adv.serviceData]}

setByte :: Int -> Word8 -> BS.ByteString -> BS.ByteString
setByte offset byte value = BS.take offset value <> BS.singleton byte <> BS.drop (offset + 1) value
