# hasquitto-effectful

A thin [`effectful`](https://hackage.haskell.org/package/effectful) wrapper over
[`hasquitto-auto-reconnect`](../hasquitto-auto-reconnect). It introduces a dynamic,
IO-backed **`Mqtt`** effect whose operations mirror the auto-reconnecting MQTT v5 client
one-for-one: every `… -> IO x` becomes `(Mqtt :> es) => … -> Eff es x`. Run it with
`runMqtt` (which needs `IOE`).

```haskell
import Effectful
import Effectful.Network.Mqtt qualified as Mqtt

main :: IO ()
main = runEff . Mqtt.runMqtt $ do
  let opts = Mqtt.defaultConnectOptions
               (Mqtt.tcpConnection (Mqtt.clientSettings "localhost" 1883)) "my-client"
  Mqtt.withClient opts Mqtt.defaultAutoReconnectConfig \client _session -> do
    _ <- Mqtt.subscribe1 client myFilter QoS1   -- tracked; replayed after reconnect
    Mqtt.publish_ client myTopic "hello"         -- blocks until connected
    forever (Mqtt.recvMessage client >>= handle) -- consumer loop spans reconnects
```

`Effectful.Network.Mqtt` re-exports the configuration/handle types from
`Network.Mqtt.Client.AutoReconnect` and the protocol vocabulary from `hasquitto-core`
(QoS, topics/filters, reason codes, properties, wills, subscriptions, the message type, the
exception hierarchy, and the TCP transport), so a caller imports only this one module.

## Notes & caveats

- **Behaviour is inherited.** Blocking-until-reconnected, in-flight error surfacing,
  resubscribe-on-fresh-session, and the receive caveats are exactly those of
  `Network.Mqtt.Client.AutoReconnect` — see its Haddock.
- **IO hooks stay IO.** `AutoReconnectConfig`'s `onReconnect` / `onResubscribe` are `IO ()`
  callbacks on the underlying record and are re-exported unchanged (they run on detached
  threads in `IO` regardless).
- **`recvMessageSTM` is re-exported unchanged** as the pure `AutoClient -> STM Message`; run
  it with an STM-capable effect (e.g. `Effectful.Concurrent.STM.atomically`).
- **Custom interpreters.** The `Mqtt` constructors are exported, so you can supply your own
  interpreter (e.g. a mock for tests) instead of `runMqtt`.

## Tests

- Unit (no broker): `cabal test hasquitto-effectful:hasquitto-effectful-test` — a stub
  interpreter checks that the smart constructors dispatch the expected operations.
- Integration (needs a broker, e.g. `eclipse-mosquitto`): `cabal test
  hasquitto-effectful:hasquitto-effectful-integration`. Endpoint is configurable via
  `--mqtt-host` / `--mqtt-port` (default `localhost:1883`).

## Copyright

2026-present (c) Hiromi ISHII
