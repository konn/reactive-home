{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Home.Reactive.SwitchBotTest (test_switchBotConfig, test_sensorNetwork, test_sensorDelivery, test_mqttRelay) where

import Control.Exception (throwIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Aeson qualified as A
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Effectful (Eff, liftIO, runEff, runPureEff, (:>))
import Effectful.Concurrent (runConcurrent)
import Effectful.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVar)
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Network.Mqtt qualified as MQTT
import Effectful.State.Static.Local (State, execState, modify)
import FRP.Rhine (ClSF, Clock (..), Result (..), TimeInfo (..), stepAutomaton)
import Home.Reactive.App (Config (..))
import Home.Reactive.Duration (seconds)
import Home.Reactive.Metrics.Hometrics
import Home.Reactive.Sensor
import Home.Reactive.SwitchBot
import Home.Reactive.SwitchBot.Runtime (deliverPending)
import Network.SwitchBot.Advertisement
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Toml qualified

test_switchBotConfig :: TestTree
test_switchBotConfig =
  testGroup
    "SwitchBot TOML"
    [ testCase "concise named sensors need no MQTT configuration" $ case Toml.decodeExact (Toml.genericCodec @Config) configText of
        Left err -> assertFailure $ show err
        Right cfg -> do
          cfg.host @?= Nothing
          cfg.port @?= Nothing
          cfg.hometrics @?= Just (HometricsConfig "http://localhost:8080/custom/readings")
          cfg.switchbot @?= Just sensorConfig
    , testCase "default timings are practical for advertisements" $ do
        scanWindow sensorConfig @?= seconds 5
        reportInterval sensorConfig @?= seconds 60
        staleAfter sensorConfig @?= seconds 120
    , testCase "BlueZ recovery can be disabled per scanner" $
        case Toml.decodeExact (Toml.genericCodec @Config) $ T.replace "[switchbot]" "[switchbot]\nbluez_recovery = false" configText of
          Left err -> assertFailure $ show err
          Right cfg -> (fmap (.bluez_recovery) cfg.switchbot) @?= Just (Just False)
    , testCase "optional MQTT relay and durations parse" $ do
        let text =
              T.replace "[switchbot]" "[switchbot]\nscan_window = \"2s\"\nreport_interval = \"10s\"\nstale_after = \"1m\"" configText
                <> "\nco2 = { id = \"11:22:33:44:55:66\", mqtt_topic = \"home/air/quality\" }\n"
        case Toml.decodeExact (Toml.genericCodec @Config) text of
          Left err -> assertFailure $ show err
          Right cfg ->
            (fmap (\c -> (mqttRelayTopics c, scanWindow c, reportInterval c, staleAfter c)) cfg.switchbot)
              @?= Just (Map.singleton "co2" "home/air/quality", seconds 2, seconds 10, seconds 60)
    , testCase "options without a relay also need no MQTT configuration" $
        case Toml.decodeExact (Toml.genericCodec @Config) $ sensorOption "id = \"aa:bb:cc:dd:ee:ff\"" of
          Left err -> assertFailure $ show err
          Right cfg -> do
            cfg.host @?= Nothing
            (mqttRelayTopics <$> cfg.switchbot) @?= Just Map.empty
            ((.sensors) <$> cfg.switchbot) @?= Just (Map.singleton "room" $ sensorOptions "AABBCCDDEEFF" Nothing)
    , testCase "sensor subtable form supports the same relay options" $
        case Toml.decodeExact (Toml.genericCodec @Config) $ T.replace "room = \"aa:bb:cc:dd:ee:ff\"" "[switchbot.sensors.room]\nid = \"aa:bb:cc:dd:ee:ff\"\nmqtt_topic = \"home/air\"" configText of
          Left err -> assertFailure $ show err
          Right cfg -> (mqttRelayTopics <$> cfg.switchbot) @?= Just (Map.singleton "room" "home/air")
    , testCase "mixed sensor forms round-trip exactly" $ do
        let cfg = sensorConfig {sensors = Map.insert "co2" (sensorOptions "112233445566" $ Just "home/air") sensorConfig.sensors}
        Toml.decodeExact switchBotConfigCodec (Toml.encode switchBotConfigCodec cfg) @?= Right cfg
    , testCase "per-sensor Hometrics name and measurement selection parse" $
        case Toml.decodeExact (Toml.genericCodec @Config) $ sensorOption "id = \"aabbccddeeff\", hometrics_name = \"Living Room\", hometrics_fields = [\"co2\"]" of
          Left err -> assertFailure $ show err
          Right cfg -> do
            (hometricsSensorConfigs <$> cfg.switchbot)
              @?= Just (Map.singleton "room" $ HometricsSensorConfig (Just "Living Room") $ Just [CO2])
            (mqttRelayTopics <$> cfg.switchbot) @?= Just Map.empty
            Toml.decodeExact (Toml.genericCodec @Config) (Toml.encode (Toml.genericCodec @Config) cfg) @?= Right cfg
    , testCase "empty Hometrics fields disable only that destination" $
        case Toml.decodeExact (Toml.genericCodec @Config) $ sensorOption "id = \"aabbccddeeff\", mqtt_topic = \"home/air\", hometrics_fields = []" of
          Left err -> assertFailure $ show err
          Right cfg -> do
            (fmap hometricsFields . hometricsSensorConfigs <$> cfg.switchbot) @?= Just (Map.singleton "room" [])
            (mqttRelayTopics <$> cfg.switchbot) @?= Just (Map.singleton "room" "home/air")
    , testCase "different fields may share one Hometrics name" $
        case Toml.decodeExact (Toml.genericCodec @Config) $
          sensorOption "id = \"aabbccddeeff\", hometrics_name = \"living\", hometrics_fields = [\"temperature\", \"humidity\"]"
            <> "\nco2 = { id = \"112233445566\", hometrics_name = \"living\", hometrics_fields = [\"co2\"] }\n" of
          Left err -> assertFailure $ show err
          Right cfg ->
            (fmap hometricsFields . hometricsSensorConfigs <$> cfg.switchbot)
              @?= Just (Map.fromList [("room", [Temperature, Humidity]), ("co2", [CO2])])
    , testGroup
        "reject invalid settings"
        [ testCase label $ case Toml.decodeExact (Toml.genericCodec @Config) text of
            Left _ -> pure ()
            Right value -> assertFailure $ "unexpected successful parse: " <> show value
        | (label, text) <-
            [ ("bad MAC", T.replace "aa:bb:cc:dd:ee:ff" "wrong" configText)
            , ("duplicate MAC", configText <> "\nother = \"AABBCCDDEEFF\"\n")
            , ("bad name", T.replace "room =" "\"room/inside\" =" configText)
            , ("empty sensors", "[switchbot]\n")
            , ("wildcard topic", sensorOption "id = \"aabbccddeeff\", mqtt_topic = \"bad/+\"")
            , ("multilevel wildcard topic", sensorOption "id = \"aabbccddeeff\", mqtt_topic = \"bad/#\"")
            , ("empty topic", sensorOption "id = \"aabbccddeeff\", mqtt_topic = \"\"")
            , ("wrong topic type", sensorOption "id = \"aabbccddeeff\", mqtt_topic = false")
            , ("missing device ID", sensorOption "mqtt_topic = \"home/air\"")
            , ("unknown sensor option", sensorOption "id = \"aabbccddeeff\", mqtt_topci = \"home/air\"")
            , ("duplicate ID across forms", configText <> "\nother = { id = \"AABBCCDDEEFF\", mqtt_topic = \"home/air\" }\n")
            , ("wrong recovery type", option "bluez_recovery = \"yes\"")
            , ("global relay option", option "mqtt_prefix = \"switchbot\"")
            , ("unsupported Hometrics field", sensorOption "id = \"aabbccddeeff\", hometrics_fields = [\"battery\"]")
            , ("wrong Hometrics field type", sensorOption "id = \"aabbccddeeff\", hometrics_fields = \"co2\"")
            , ("wrong Hometrics name type", sensorOption "id = \"aabbccddeeff\", hometrics_name = 42")
            , ("empty Hometrics name", sensorOption "id = \"aabbccddeeff\", hometrics_name = \"  \"")
            , ("unknown Hometrics option", sensorOption "id = \"aabbccddeeff\", hometrics_field = [\"co2\"]")
            , ("overlapping Hometrics destination", configText <> "\nother = { id = \"112233445566\", hometrics_name = \"room\", hometrics_fields = [\"co2\"] }\n")
            , ("zero scan", option "scan_window = \"0s\"")
            , ("negative scan", option "scan_window = \"-1s\"")
            , ("very long scan", option "scan_window = \"61s\"")
            , ("bad duration", option "report_interval = \"later\"")
            , ("report after expiry", option "report_interval = \"3m\"")
            , ("non-finite duration", option $ "stale_after = \"" <> T.replicate 400 "9" <> "s\"")
            , ("wrong sensors type", T.replace "[switchbot.sensors]\nroom = \"aa:bb:cc:dd:ee:ff\"" "sensors = 42" configText)
            ]
        ]
    ]
  where
    option value = T.replace "[switchbot]" ("[switchbot]\n" <> value) configText
    sensorOption value = T.replace "room = \"aa:bb:cc:dd:ee:ff\"" ("room = { " <> value <> " }") configText

configText :: T.Text
configText =
  """
  [hometrics]
  endpoint = "http://localhost:8080/custom/readings"

  [switchbot]
  [switchbot.sensors]
  room = "aa:bb:cc:dd:ee:ff"
  """

sensorConfig :: SwitchBotConfig
sensorConfig = SwitchBotConfig (Map.singleton "room" $ SensorId "AABBCCDDEEFF") Nothing Nothing Nothing Nothing Nothing

sensorOptions :: T.Text -> Maybe T.Text -> SwitchBotSensor
sensorOptions device topic = SensorOptions $ SwitchBotSensorOptions device topic defaultHometricsSensorConfig

reading :: SensorReading
reading = SensorReading "AABBCCDDEEFF" MeterProCO2 (Just 25.3) (Just 66) (Just 584) (Just 100) Nothing

sample :: UTCTime -> SensorSample
sample now = SensorSample "room" now reading

baseTime :: UTCTime
baseTime = read "2026-09-21 00:00:00 UTC"

data TestClock = TestClock

instance Clock (Eff es) TestClock where
  type Time TestClock = UTCTime
  type Tag TestClock = ()
  initClock TestClock = error "TestClock is manually stepped"

runSignal :: ClSF (Eff es) TestClock a b -> [(Double, a)] -> Eff es [b]
runSignal initial = go initial Nothing
  where
    go _ _ [] = pure []
    go signal previous ((at, input) : rest) = do
      let info = TimeInfo (maybe 0 (at -) previous) at (addUTCTime (realToFrac at) baseTime) ()
      Result signal' output <- runReaderT (stepAutomaton signal input) info
      (output :) <$> go signal' (Just at) rest

test_sensorNetwork :: TestTree
test_sensorNetwork =
  testGroup
    "sensor Rhine network"
    [ testCase "filters unconfigured devices and emits named timestamped samples" $ do
        let updates = runPureEff $ runSignal (switchBotS sensorConfig) [(0, [reading, reading {deviceId = "112233445566"}]), (1, [])]
        map (.samples) updates @?= [[sample baseTime], []]
        map (Map.size . (.snapshot)) updates @?= [1, 1]
    , testCase "Hometrics projection leaves full readings and Rhine names intact" $ do
        let options = SwitchBotSensorOptions "AABBCCDDEEFF" Nothing $ HometricsSensorConfig (Just "Living Room") $ Just [CO2]
            cfg = sensorConfig {sensors = Map.singleton "room" $ SensorOptions options}
            updates = runPureEff $ runSignal (switchBotS cfg) [(0, [reading])]
        map (.samples) updates @?= [[sample baseTime]]
    , testCase "heartbeat with no BLE data expires snapshots at the boundary" $ do
        let snapshots = runPureEff $ runSignal (sensorSnapshotS $ seconds 10) [(0, [sample baseTime]), (9.9, []), (10, [])]
        map Map.size snapshots @?= [1, 1, 0]
    , testCase "fresh advertisements refresh even unchanged measurements" $ do
        let updates = runPureEff $ runSignal (switchBotS sensorConfig) [(0, [reading]), (100, [reading]), (200, []), (220, [])]
        map (Map.size . (.snapshot)) updates @?= [1, 1, 1, 0]
    , testCase "acknowledging old readings preserves newer pending observations" $ do
        let old = Map.singleton "room" $ sample baseTime
            new = Map.singleton "room" $ sample $ addUTCTime 1 baseTime
        pendingAfterDelivery old new @?= new
        pendingAfterDelivery old old @?= Map.empty
    ]

test_sensorDelivery :: TestTree
test_sensorDelivery = testCase "failed delivery retries, concurrent updates survive, stale values expire" $ runEff $ runConcurrent $ do
  now <- liftIO getCurrentTime
  pending <- newTVarIO $ Map.singleton "room" $ sample now
  errors <- newTVarIO ([] :: [T.Text])
  let report err = atomically $ modifyTVar' errors (<> [err])
      ttl = seconds 120
      check expected = do
        actual <- atomically $ readTVar pending
        liftIO $ actual @?= expected
  deliverPending ttl pending (\_ -> liftIO $ throwIO $ userError "offline") report
  check $ Map.singleton "room" $ sample now
  capturedErrors <- atomically $ readTVar errors
  liftIO $ assertBool "failure is observable" $ not $ null capturedErrors
  let newer = sample $ addUTCTime 1 now
  deliverPending ttl pending (\_ -> atomically $ modifyTVar' pending $ Map.insert "room" newer) report
  check $ Map.singleton "room" newer
  deliverPending ttl pending (\_ -> pure ()) report
  check Map.empty
  atomically $ modifyTVar' pending $ Map.insert "room" $ sample $ addUTCTime (-121) now
  deliverPending ttl pending (\_ -> liftIO $ assertFailure "stale sample was delivered") report
  check Map.empty

test_mqttRelay :: TestTree
test_mqttRelay =
  testGroup
    "per-sensor MQTT relay"
    [ testCase "only opted-in sensors publish to their exact topics" $ do
        let cfg =
              sensorConfig
                { sensors =
                    Map.fromList
                      [ ("room", SensorOptions $ SwitchBotSensorOptions "AABBCCDDEEFF" (Just "home/living/air") $ HometricsSensorConfig (Just "Living Room") $ Just [CO2])
                      , ("outdoor", sensorOptions "112233445566" $ Just "garden/climate")
                      , ("private", SensorId "112233445577")
                      , ("local", sensorOptions "112233445588" Nothing)
                      ]
                }
            named name = (sample baseTime) {sensor = name}
            result = capture cfg [sample baseTime, named "outdoor", named "private", named "local", named "unknown"]
        result
          @?= [ ("home/living/air", A.toJSON $ sample baseTime, MQTT.QoS1, False)
              , ("garden/climate", A.toJSON $ named "outdoor", MQTT.QoS1, False)
              ]
    , testCase "shorthand sensors never publish" $
        capture sensorConfig [sample baseTime] @?= []
    ]
  where
    capture :: SwitchBotConfig -> [SensorSample] -> [(MQTT.Topic, A.Value, MQTT.QoS, Bool)]
    capture cfg samples = runPureEff $ execState [] $ record $ publishSensorSamples cfg samples
    record :: (State [(MQTT.Topic, A.Value, MQTT.QoS, Bool)] :> es) => Eff (MQTT.Mqtt : es) a -> Eff es a
    record = interpret_ $ \case
      MQTT.Publish topic payload options -> do
        modify (<> [(topic, either error id $ A.eitherDecodeStrict @A.Value payload, options.qos, options.retain)])
        pure $ MQTT.AckedQoS1 MQTT.Success []
      _ -> error "sensor relay must only publish"
