{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Home.Reactive.Metrics.HometricsTest (test_hometricsPayload, test_hometricsHTTP) where

import Control.Exception (SomeException, try)
import Data.Aeson qualified as A
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Home.Reactive.Metrics.Hometrics
import Home.Reactive.Sensor
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (status204, status302, status400, status502)
import Network.SwitchBot.Advertisement
import Network.Wai (rawPathInfo, requestHeaders, requestMethod, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

sample :: SensorSample
sample =
  SensorSample "room" (read "2026-09-21 00:00:00 UTC") $
    SensorReading "AABBCCDDEEFF" MeterProCO2 (Just 25.3) (Just 66) (Just 584) (Just 100) Nothing

expected :: A.Value
expected =
  A.object
    [ "temperatures" A..= A.object ["room" A..= (25.3 :: Double)]
    , "humidity" A..= A.object ["room" A..= (66 :: Int)]
    , "co2" A..= A.object ["room" A..= (584 :: Int)]
    ]

defaultSensors :: Map.Map T.Text HometricsSensorConfig
defaultSensors = Map.singleton "room" defaultHometricsSensorConfig

co2Only :: Map.Map T.Text HometricsSensorConfig
co2Only = Map.singleton "room" $ HometricsSensorConfig (Just "Living Room") $ Just [CO2]

expectedCO2 :: A.Value
expectedCO2 = A.object ["co2" A..= A.object ["Living Room" A..= (584 :: Int)]]

test_hometricsPayload :: TestTree
test_hometricsPayload =
  testGroup
    "Hometrics REST payload"
    [ testCase "uses named Celsius, humidity percent and CO2 ppm maps" $ hometricsPayload defaultSensors [sample] @?= Just expected
    , testCase "empty input sends no request" $ hometricsPayload defaultSensors [] @?= Nothing
    , testCase "CO2-only input omits empty maps" $
        hometricsPayload defaultSensors [sample {reading = sample.reading {temperatureC = Nothing, humidityPercent = Nothing}}]
          @?= Just (A.object ["co2" A..= A.object ["room" A..= (584 :: Int)]])
    , testCase "unavailable readings and metadata cannot become zero measurements" $
        hometricsPayload defaultSensors [sample {reading = sample.reading {temperatureC = Nothing, humidityPercent = Nothing, co2Ppm = Nothing}}] @?= Nothing
    , testCase "renames and sends only the selected measurements" $
        hometricsPayload co2Only [sample] @?= Just expectedCO2
    , testCase "disabled and unconfigured sensors send no measurements" $ do
        let disabled = Map.singleton "room" $ HometricsSensorConfig Nothing $ Just []
        hometricsPayload disabled [sample] @?= Nothing
        hometricsPayload Map.empty [sample] @?= Nothing
    , testCase "unavailable selected measurement does not fall back to other fields" $
        hometricsPayload co2Only [sample {reading = sample.reading {co2Ppm = Nothing}}] @?= Nothing
    , testCase "disjoint measurements can combine under one Hometrics name" $ do
        let sensors = Map.insert "climate" (HometricsSensorConfig (Just "Living Room") $ Just [Temperature, Humidity]) co2Only
            climate = sample {sensor = "climate", reading = sample.reading {temperatureC = Just 20, humidityPercent = Just 80, co2Ppm = Just 999}}
        hometricsPayload sensors [sample, climate]
          @?= Just
            ( A.object
                [ "temperatures" A..= A.object ["Living Room" A..= (20 :: Double)]
                , "humidity" A..= A.object ["Living Room" A..= (80 :: Int)]
                , "co2" A..= A.object ["Living Room" A..= (584 :: Int)]
                ]
            )
    ]

test_hometricsHTTP :: TestTree
test_hometricsHTTP =
  testGroup
    "Hometrics local HTTP"
    [ testCase "POSTs JSON to the configured custom path and accepts 204" $ do
        captured <- newIORef Nothing
        let app req respond = do
              body <- strictRequestBody req
              writeIORef captured $ Just (requestMethod req, rawPathInfo req, lookup "Content-Type" $ requestHeaders req, A.eitherDecode @A.Value body)
              respond $ responseLBS status204 [] ""
        manager <- newManager defaultManagerSettings
        testWithApplication (pure app) $ \port -> postHometrics manager (endpointAt port) co2Only [sample]
        readIORef captured >>= (@?= Just ("POST", "/custom/readings", Just "application/json", Right expectedCO2))
    , testCase "an opted-out sensor does not make an HTTP request" $ do
        manager <- newManager defaultManagerSettings
        let disabled = Map.singleton "room" $ HometricsSensorConfig Nothing $ Just []
        postHometrics manager (HometricsConfig "not a URL") disabled [sample]
    , testGroup
        "rejects failures and redirects"
        [ testCase (show status) $ do
            manager <- newManager defaultManagerSettings
            let app _ respond = respond $ responseLBS status [("Location", "/elsewhere")] ""
            testWithApplication (pure app) $ \port -> do
              result <- try @SomeException $ postHometrics manager (endpointAt port) defaultSensors [sample]
              assertBool "must fail so the batch remains pending" $ either (const True) (const False) result
        | status <- [status302, status400, status502]
        ]
    ]
  where
    endpointAt port = HometricsConfig $ "http://127.0.0.1:" <> T.pack (show port) <> "/custom/readings"
