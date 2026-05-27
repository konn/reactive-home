{- | Integration tests for the auto-reconnect wrapper against a real MQTT broker.

The broker endpoint is configurable via the @--mqtt-host@ and @--mqtt-port@
command-line options (default @localhost:1883@); tasty also accepts them through
the @TASTY_MQTT_HOST@ \/ @TASTY_MQTT_PORT@ environment variables. A broker must be
running at that endpoint for these tests to pass.

Connections are forced to drop by /session takeover/: a second client connecting
with the same client identifier makes the broker disconnect the first (MQTT
§3.1.4) — a clean way to exercise the supervisor without low-level socket surgery.
-}
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (poll, wait, withAsync)
import Control.Concurrent.STM
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Mqtt.Client
import Network.Mqtt.Client.AutoReconnect (AutoReconnectConfig (..), BackoffConfig (..), Status (..), defaultAutoReconnectConfig)
import Network.Mqtt.Client.AutoReconnect qualified as Auto
import Network.Mqtt.Connection (Conn)
import Network.Mqtt.Connection.TCP (clientSettings, tcpConnection)
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
    "auto-reconnect integration (mosquitto)"
    [ testCase "auto-reconnects and resubscribes after a session takeover" do
        cid <- uniqueClientId "ar-takeover"
        reconnected <- newTVarIO (0 :: Int)
        let cfg =
              defaultAutoReconnectConfig
                { onReconnect = Just \_ _ -> atomically (modifyTVar' reconnected (+ 1))
                , backoff = BackoffConfig 50_000 500_000
                }
        Auto.withClient (defaultConnectOptions factory cid) cfg \a _ -> do
          t <- uniqueTopic
          tf <- either (error . show) pure (mkTopicFilter t.raw)
          rc <- Auto.subscribe1 a tf QoS1
          assertBool ("SUBACK reason " <> show rc) (isSuccess rc)
          -- Take over A's client id to make the broker disconnect it.
          (b, _) <- connect (defaultConnectOptions factory cid)
          waited <- timeout 15_000_000 (atomically (readTVar reconnected >>= \n -> when (n < 1) retry))
          disconnect b
          case waited of
            Nothing -> assertFailure "A did not auto-reconnect within 15s after takeover"
            Just () -> pure ()
          -- A is back and (since the new session is fresh) resubscribed: round-trip a message
          -- to prove the subscription was replayed. (Blocking during the reconnect window is
          -- covered by the dedicated test below.)
          _ <- Auto.publish a t payload (pubOpts QoS1)
          received <- timeout 5_000_000 (Auto.recvMessage a)
          case received of
            Just msg -> msg.payload @?= payload
            Nothing -> assertFailure "no message after reconnect (resubscribe failed?)"
    , testCase "blocks operations during the reconnect window instead of throwing" do
        cid <- uniqueClientId "ar-block"
        calls <- newTVarIO (0 :: Int)
        gate <- newTVarIO False
        let
          -- Succeed on the initial connect; on the first reconnect, park in the factory
          -- until the test opens the gate, holding A in the Reconnecting state.
          gatedFactory :: IO Conn
          gatedFactory = do
            n <- atomically (stateTVar calls \k -> (k, k + 1))
            when (n >= 1) (atomically (readTVar gate >>= \g -> unless g retry))
            factory
          cfg = defaultAutoReconnectConfig {backoff = BackoffConfig 0 0} -- the gate, not a sleep, is the window
        Auto.withClient (defaultConnectOptions gatedFactory cid) cfg \a _ -> do
          t <- uniqueTopic
          tf <- either (error . show) pure (mkTopicFilter t.raw)
          _ <- Auto.subscribe1 a tf QoS1
          (b, _) <- connect (defaultConnectOptions factory cid) -- takeover → A drops
          parked <- timeout 15_000_000 (atomically (readTVar calls >>= \c -> when (c < 2) retry))
          case parked of
            Nothing -> assertFailure "supervisor never attempted to reconnect"
            Just () -> pure ()
          down <- Auto.isConnected a
          assertBool "link should be observably down while reconnecting" (not down)
          -- A publish issued now must BLOCK (not throw) until the gate releases.
          withAsync (Auto.publish a t payload (pubOpts QoS1)) \pub -> do
            threadDelay 300_000
            early <- poll pub
            case early of
              Just _ -> assertFailure "publish did not block during the reconnect window"
              Nothing -> pure ()
            atomically (writeTVar gate True) -- let the reconnect proceed
            res <- timeout 15_000_000 (wait pub)
            case res of
              Nothing -> assertFailure "publish never completed after reconnect"
              Just r -> assertBool ("publish result " <> show r) (publishOk r)
          received <- timeout 5_000_000 (Auto.recvMessage a)
          case received of
            Just msg -> msg.payload @?= payload
            Nothing -> assertFailure "no message after the blocked publish completed"
          disconnect b
    , testCase "supervisor survives transient transport failures" do
        cid <- uniqueClientId "ar-failingfactory"
        attempts <- newIORef (0 :: Int)
        reconnected <- newTVarIO (0 :: Int)
        let
          -- Succeed on the very first connect (call 0), then throw on the next two
          -- reconnect attempts, then succeed again.
          failingFactory :: IO Conn
          failingFactory = do
            n <- atomicModifyIORef' attempts \k -> (k + 1, k)
            if n >= 1 && n <= 2
              then ioError (userError "simulated transport failure")
              else factory
          cfg =
            defaultAutoReconnectConfig
              { onReconnect = Just \_ _ -> atomically (modifyTVar' reconnected (+ 1))
              , backoff = BackoffConfig 20_000 100_000
              }
        Auto.withClient (defaultConnectOptions failingFactory cid) cfg \a _ -> do
          (b, _) <- connect (defaultConnectOptions factory cid) -- takeover → drop A
          waited <- timeout 15_000_000 (atomically (readTVar reconnected >>= \n -> when (n < 1) retry))
          disconnect b
          case waited of
            Nothing -> assertFailure "supervisor did not recover from transient failures (possibly wedged)"
            Just () -> do
              n <- readIORef attempts
              assertBool ("factory called " <> show n <> " times, expected the failures to be retried") (n >= 4)
              s <- Auto.status a
              s @?= Connected
    , testCase "intentional disconnect does not reconnect" do
        cid <- uniqueClientId "ar-disconnect"
        (a, _) <- Auto.connect (defaultConnectOptions factory cid) defaultAutoReconnectConfig
        t <- uniqueTopic
        tf <- either (error . show) pure (mkTopicFilter t.raw)
        _ <- Auto.subscribe1 a tf QoS0
        Auto.disconnect a
        closed <- timeout 5_000_000 (Auto.waitClosed a)
        case closed of
          Just (Right rc) -> rc @?= NormalDisconnection
          other -> assertFailure ("expected Right NormalDisconnection, got " <> show other)
        threadDelay 500_000 -- a (cancelled) supervisor would have tried to reconnect by now
        s <- Auto.status a
        s @?= Closed
    ]
  where
    factory :: IO Conn
    factory = tcpConnection (clientSettings host (fromIntegral port))
    payload :: ByteString
    payload = "auto-reconnect-payload"

pubOpts :: QoS -> PublishOptions
pubOpts q = PublishOptions {qos = q, retain = False, properties = []}

publishOk :: PublishResult -> Bool
publishOk = \case
  PublishedQoS0 -> True
  AckedQoS1 rc _ -> isSuccess rc
  AckedQoS2 rc _ -> isSuccess rc

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
  either (error . show) pure (mkTopic (T.pack ("hasquitto/ar-itest/" <> s)))
