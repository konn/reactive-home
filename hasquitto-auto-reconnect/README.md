# hasquitto-auto-reconnect

An auto-reconnecting, auto-resubscribing MQTT v5 client, layered entirely on the
public API of [`hasquitto-core`](../hasquitto-core). The core stays low-level; this
package adds the convenience (and the not-so-low-level subscription bookkeeping) on
top.

A background supervisor watches the connection and, when it drops unexpectedly,
re-establishes it with **full-jitter exponential backoff**, then replays the
subscriptions you made through this module. Messaging operations **block until
reconnected** instead of failing during the reconnect window.

```haskell
import Network.Mqtt.Client.AutoReconnect qualified as Auto
import Network.Mqtt.Client (defaultConnectOptions)
import Network.Mqtt.Connection.TCP (clientSettings, tcpConnection)

main :: IO ()
main = do
  let opts = defaultConnectOptions (tcpConnection (clientSettings "localhost" 1883)) "my-client"
  Auto.withClient opts Auto.defaultAutoReconnectConfig \client _session -> do
    _ <- Auto.subscribe1 client myFilter QoS1   -- tracked; replayed after reconnect
    Auto.publish_ client myTopic "hello"         -- blocks until connected
    forever (Auto.recvMessage client >>= handle) -- consumer loop spans reconnects
```

## Behaviour & caveats

- **Block vs. surface.** A call issued *while disconnected* blocks until the link
  is back. A call already *in flight* when the link drops surfaces its
  connection-lost error to that one caller; the wrapper never silently re-issues it.
- **In-flight QoS across a reconnect.** The core engine replays in-flight QoS 1/2
  publishes only when the reconnect *resumes the same session*
  (`cleanStart = False` + a non-zero Session Expiry). With the default
  `cleanStart = True` an interrupted publish is not auto-redelivered.
- **Resubscribe.** After a fresh-session reconnect the tracked subscriptions are
  replayed in one SUBSCRIBE. Filters the broker rejects are dropped and reported via
  `onResubscribe`; the client still becomes `Connected`.
- **Receiving across a fresh-session reconnect.** Consume messages promptly: a QoS
  1/2 message received-but-unconsumed before a fresh-session reconnect carries a
  stale-acknowledgement caveat (a core-level limitation this layer cannot fix).

See the `Network.Mqtt.Client.AutoReconnect` module Haddock for the full details.

## Tests

- Unit/property: `cabal test hasquitto-auto-reconnect:hasquitto-auto-reconnect-test`.
- Integration (needs a broker, e.g. `eclipse-mosquitto`): `cabal test
  hasquitto-auto-reconnect:hasquitto-auto-reconnect-integration`. Endpoint is
  configurable via `--mqtt-host` / `--mqtt-port` (default `localhost:1883`).

## Copyright

2026-present (c) Hiromi ISHII
