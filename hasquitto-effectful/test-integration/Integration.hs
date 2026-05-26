{- | Integration tests for the 'Mqtt' effect against a real MQTT broker.

These mirror a representative subset of @hasquitto-auto-reconnect@'s integration suite, driving
the client-under-test through the effect: a client is acquired with 'withClient' \/ 'runMqtt'
and the effect is scoped with 'runMqttWith', so MQTT calls read as @subscribe1 f q@ /
@publish t b o@ with no explicit handle. 'liftIO' carries the non-MQTT orchestration
(assertions, timeouts, STM counters, forcing a takeover via a second /raw/
"Network.Mqtt.Client" connection). The timeout-bounded receive uses the re-exported pure
'recvMessageSTM' applied to the client from 'getClient'.

The broker endpoint is configurable via @--mqtt-host@ \/ @--mqtt-port@ (default
@localhost:1883@), also via @TASTY_MQTT_HOST@ \/ @TASTY_MQTT_PORT@. A broker must be running
at that endpoint for these tests to pass.
-}
module Main (main) where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVar, retry)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Effectful (liftIO, runEff)
import Effectful.Network.Mqtt
import Network.Mqtt.Client qualified as Core
import Network.Mqtt.Client.AutoReconnect qualified as Auto
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
    "hasquitto-effectful integration (mosquitto)"
    [ testCase "withClient + runMqttWith: auto-reconnects and resubscribes after a session takeover" do
        cid <- uniqueClientId "eff-takeover"
        reconnected <- newTVarIO (0 :: Int)
        let cfg =
              defaultAutoReconnectConfig
                { onReconnect = Just \_ _ -> atomically (modifyTVar' reconnected (+ 1))
                , backoff = BackoffConfig 50_000 500_000
                }
        done <- timeout 30_000_000 $
          runEff $
            withClient (opts cid) cfg \a session -> runMqttWith a session do
              t <- liftIO uniqueTopic
              tf <- liftIO (either (error . show) pure (mkTopicFilter t.raw))
              rc <- subscribe1 tf QoS1
              liftIO (assertBool ("SUBACK reason " <> show rc) (isSuccess rc))
              -- Take over A's client id so the broker disconnects it.
              (b, _) <- liftIO (Core.connect (opts cid))
              waited <- liftIO (timeout 15_000_000 (atomically (readTVar reconnected >>= \n -> when (n < 1) retry)))
              liftIO (Core.disconnect b)
              liftIO (maybe (assertFailure "A did not auto-reconnect within 15s after takeover") pure waited)
              -- A is back and resubscribed (fresh session): round-trip a message to prove it.
              _ <- publish t payload (pubOpts QoS1)
              a' <- getClient
              received <- liftIO (timeout 5_000_000 (atomically (recvMessageSTM a')))
              liftIO case received of
                Just msg -> msg.payload @?= payload
                Nothing -> assertFailure "no message after reconnect (resubscribe failed?)"
        maybe (assertFailure "takeover test timed out after 30s") pure done
    , testCase "runMqtt: subscribe -> publish -> receive round-trip" do
        cid <- uniqueClientId "eff-roundtrip"
        done <- timeout 20_000_000 $
          runEff $
            runMqtt (opts cid) defaultAutoReconnectConfig do
              t <- liftIO uniqueTopic
              tf <- liftIO (either (error . show) pure (mkTopicFilter t.raw))
              rc <- subscribe1 tf QoS1
              liftIO (assertBool ("SUBACK reason " <> show rc) (isSuccess rc))
              _ <- publish t payload (pubOpts QoS1)
              a <- getClient
              received <- liftIO (timeout 5_000_000 (atomically (recvMessageSTM a)))
              -- runMqtt does not disconnect; clean up the supervisor ourselves via the held client.
              liftIO (Auto.disconnect a)
              pure received
        case done of
          Just (Just msg) -> msg.payload @?= payload
          Just Nothing -> assertFailure "no message received on the round-trip"
          Nothing -> assertFailure "round-trip test timed out after 20s"
    ]
  where
    factory :: IO Conn
    factory = tcpConnection (clientSettings host (fromIntegral port))
    opts :: T.Text -> ConnectOptions
    opts = defaultConnectOptions factory
    payload :: ByteString
    payload = "hasquitto-effectful-payload"

pubOpts :: QoS -> PublishOptions
pubOpts q = PublishOptions {qos = q, retain = False, properties = []}

uniqueClientId :: String -> IO T.Text
uniqueClientId label = do
  s <- uniqueSuffix
  pure (T.pack ("hasquitto-" <> label <> "-" <> s))

uniqueSuffix :: IO String
uniqueSuffix = do
  t <- getPOSIXTime
  pure (show (round (t * 1e6) :: Integer))

uniqueTopic :: IO Topic
uniqueTopic = do
  s <- uniqueSuffix
  either (error . show) pure (mkTopic (T.pack ("hasquitto/eff-itest/" <> s)))
