{- | Integration tests against a real MQTT broker.

The broker endpoint is configurable via the @--mqtt-host@ and @--mqtt-port@
command-line options (default @localhost:1883@); tasty also accepts them through
the @TASTY_MQTT_HOST@ / @TASTY_MQTT_PORT@ environment variables. A broker must be
running at that endpoint for these tests to pass.
-}
module Main (main) where

import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Mqtt.Client
import Network.Mqtt.Connection (Conn)
import Network.Mqtt.Connection.TCP (clientSettings, tcpConnection)
import Network.Mqtt.Message (Message (..))
import Network.Mqtt.Types
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.Options (IsOption (..), OptionDescription (Option), safeRead)
import Test.Tasty.Runners (NumThreads (..))

-- | Broker host (option @--mqtt-host@, env @TASTY_MQTT_HOST@).
newtype MqttHost = MqttHost String

instance IsOption MqttHost where
  defaultValue = MqttHost "localhost"
  parseValue = Just . MqttHost
  optionName = pure "mqtt-host"
  optionHelp = pure "Hostname of the MQTT broker for integration tests (default: localhost)"

-- | Broker port (option @--mqtt-port@, env @TASTY_MQTT_PORT@).
newtype MqttPort = MqttPort Int

instance IsOption MqttPort where
  defaultValue = MqttPort 1883
  parseValue = fmap MqttPort . safeRead
  optionName = pure "mqtt-port"
  optionHelp = pure "Port of the MQTT broker for integration tests (default: 1883)"

main :: IO ()
main =
  defaultMainWithIngredients
    (includingOptions [Option (Proxy :: Proxy MqttHost), Option (Proxy :: Proxy MqttPort)] : defaultIngredients)
    ( localOption (NumThreads 1) $
        askOption \(MqttHost host) ->
          askOption \(MqttPort port) ->
            mkTests host port
    )

mkTests :: String -> Int -> TestTree
mkTests host port =
  testGroup
    "integration (mosquitto)"
    [ testCase "connect handshake succeeds with a clean reason code" do
        opts <- baseOptions
        withClient opts \_ s ->
          assertBool ("CONNACK reason " <> show s.reasonCode) (isSuccess s.reasonCode)
    , testCase "empty client id yields a server-assigned identifier" do
        (c, s) <- connect (defaultConnectOptions factory "")
        disconnect c
        assertBool "assigned client id present" (isJust s.assignedClientId)
    , testCase "publish QoS0 is fire-and-forget" do
        opts <- baseOptions
        withClient opts \c _ -> do
          t <- uniqueTopic
          r <- publish c t "q0" (pubOpts QoS0)
          r @?= PublishedQoS0
    , testCase "publish QoS1 is acknowledged" do
        opts <- baseOptions
        withClient opts \c _ -> do
          t <- uniqueTopic
          r <- publish c t "q1" (pubOpts QoS1)
          case r of
            AckedQoS1 rc _ -> assertBool ("PUBACK reason " <> show rc) (isSuccess rc)
            other -> assertFailure ("expected AckedQoS1, got " <> show other)
    , testCase "publish QoS2 completes the handshake" do
        opts <- baseOptions
        withClient opts \c _ -> do
          t <- uniqueTopic
          r <- publish c t "q2" (pubOpts QoS2)
          case r of
            AckedQoS2 rc _ -> assertBool ("PUBCOMP reason " <> show rc) (isSuccess rc)
            other -> assertFailure ("expected AckedQoS2, got " <> show other)
    , roundTrip baseOptions "QoS1" QoS1
    , roundTrip baseOptions "QoS2" QoS2
    ]
  where
    factory :: IO Conn
    factory = tcpConnection (clientSettings host (fromIntegral port))
    baseOptions :: IO ConnectOptions
    baseOptions = do
      n <- uniqueSuffix
      pure (defaultConnectOptions factory (T.pack ("hasquitto-itest-" <> n)))

-- | Subscribe, publish to the same topic, and confirm the payload comes back.
roundTrip :: IO ConnectOptions -> String -> QoS -> TestTree
roundTrip baseOptions label q =
  testCase ("subscribe + publish + recv round-trip (" <> label <> ")") do
    opts <- baseOptions
    withClient opts \c _ -> do
      t <- uniqueTopic
      tf <- either (error . show) pure (mkTopicFilter t.raw)
      rc <- subscribe1 c tf q
      assertBool ("SUBACK reason " <> show rc) (isSuccess rc)
      _ <- publish c t payload (pubOpts q)
      received <- timeout 5_000_000 (recvMessage c)
      case received of
        Just msg -> msg.payload @?= payload
        Nothing -> assertFailure "no message received within 5s"
  where
    payload :: ByteString
    payload = "round-trip-payload"

pubOpts :: QoS -> PublishOptions
pubOpts q = PublishOptions {qos = q, retain = False, properties = []}

uniqueSuffix :: IO String
uniqueSuffix = do
  t <- getPOSIXTime
  pure (show (round (t * 1e6) :: Integer))

uniqueTopic :: IO Topic
uniqueTopic = do
  n <- uniqueSuffix
  either (error . show) pure (mkTopic (T.pack ("hasquitto/itest/" <> n)))
