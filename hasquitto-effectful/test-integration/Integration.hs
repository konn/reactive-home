{- | Integration tests for the 'Mqtt' effect against a real MQTT broker.

These mirror a representative subset of @hasquitto-auto-reconnect@'s integration suite, but
drive the client-under-test entirely through the effect: the body runs in @'runEff' .
'runMqtt'@ and calls 'connect' / 'withClient' / 'subscribe1' / 'publish' / 'waitClosed' /
'status' as effect operations, using 'liftIO' for the non-MQTT orchestration (assertions,
timeouts, STM counters, and forcing a takeover via a second /raw/ "Network.Mqtt.Client"
connection). The timeout-bounded receive uses the re-exported pure 'recvMessageSTM'.

The broker endpoint is configurable via @--mqtt-host@ \/ @--mqtt-port@ (default
@localhost:1883@), also via @TASTY_MQTT_HOST@ \/ @TASTY_MQTT_PORT@. A broker must be running
at that endpoint for these tests to pass.
-}
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVar, retry)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Effectful (liftIO, runEff)
import Effectful.Network.Mqtt
import Network.Mqtt.Client qualified as Core
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
    [ testCase "auto-reconnects and resubscribes after a session takeover (via the effect)" do
        cid <- uniqueClientId "eff-takeover"
        reconnected <- newTVarIO (0 :: Int)
        let cfg =
              defaultAutoReconnectConfig
                { onReconnect = Just \_ _ -> atomically (modifyTVar' reconnected (+ 1))
                , backoff = BackoffConfig 50_000 500_000
                }
        done <- timeout 30_000_000 $
          runEff . runMqtt $
            withClient (opts cid) cfg \a _ -> do
              t <- liftIO uniqueTopic
              tf <- liftIO (either (error . show) pure (mkTopicFilter t.raw))
              rc <- subscribe1 a tf QoS1
              liftIO (assertBool ("SUBACK reason " <> show rc) (isSuccess rc))
              -- Take over A's client id so the broker disconnects it.
              (b, _) <- liftIO (Core.connect (opts cid))
              waited <- liftIO (timeout 15_000_000 (atomically (readTVar reconnected >>= \n -> when (n < 1) retry)))
              liftIO (Core.disconnect b)
              liftIO (maybe (assertFailure "A did not auto-reconnect within 15s after takeover") pure waited)
              -- A is back and resubscribed (fresh session): round-trip a message to prove it.
              _ <- publish a t payload (pubOpts QoS1)
              received <- liftIO (timeout 5_000_000 (atomically (recvMessageSTM a)))
              liftIO case received of
                Just msg -> msg.payload @?= payload
                Nothing -> assertFailure "no message after reconnect (resubscribe failed?)"
        maybe (assertFailure "takeover test timed out after 30s") pure done
    , testCase "intentional disconnect does not reconnect (via the effect)" do
        cid <- uniqueClientId "eff-disconnect"
        done <- timeout 15_000_000 $ runEff . runMqtt $ do
          (a, _) <- connect (opts cid) defaultAutoReconnectConfig
          t <- liftIO uniqueTopic
          tf <- liftIO (either (error . show) pure (mkTopicFilter t.raw))
          _ <- subscribe1 a tf QoS0
          disconnect a
          closed <- waitClosed a
          liftIO (threadDelay 500_000) -- a (cancelled) supervisor would have tried to reconnect by now
          st <- status a
          pure (closed, st)
        case done of
          Just (Right rc, st) -> do
            rc @?= NormalDisconnection
            st @?= Closed
          Just (other, _) -> assertFailure ("expected Right NormalDisconnection, got " <> show other)
          Nothing -> assertFailure "disconnect test timed out after 15s"
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
