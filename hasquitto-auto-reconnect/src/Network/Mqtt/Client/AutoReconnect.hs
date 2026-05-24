{- | An auto-reconnecting, auto-resubscribing MQTT v5 client, layered on the
low-level "Network.Mqtt.Client" engine.

A background supervisor watches the connection and, when it drops unexpectedly,
re-establishes it with full-jitter exponential backoff, then replays the
subscriptions made through this module. Messaging operations /block until
reconnected/ instead of failing during the reconnect window. Intended for
qualified import:

@
import Network.Mqtt.Client.AutoReconnect qualified as Auto
@

== Delivery semantics & caveats

* __Block vs. surface.__ A call issued /while disconnected/ blocks until the link
  is back, then runs. A call already /in flight/ when the link drops surfaces its
  connection-lost error to that one caller; the wrapper never silently re-issues it.

* __In-flight QoS across a reconnect.__ The core engine replays in-flight QoS 1\/2
  publishes (PUBLISH @DUP=1@ \/ re-issued PUBREL) only when the reconnect resumes the
  /same/ MQTT session (@cleanStart = False@ plus a non-zero Session Expiry). With the
  default @cleanStart = True@ the session is fresh, in-flight state is discarded, and
  an interrupted publish is /not/ auto-redelivered — its caller simply sees the error.

* __Resubscribe.__ After a fresh-session reconnect the tracked subscriptions are
  replayed in one SUBSCRIBE. Filters the broker rejects (SUBACK @>= 0x80@) are dropped
  from the tracked set and reported via 'onResubscribe'; the client still becomes
  'Connected'. So 'Connected' means \"connected; subscriptions replayed best-effort\",
  never silent success.

* __Hook lifetime.__ 'onReconnect' and 'onResubscribe' run on detached threads so a
  slow or throwing hook can never wedge the supervisor. They are __not__ cancelled by
  'disconnect', so a blocking hook can outlive the client — keep hooks self-contained
  and own their own lifetime.

* __Receiving across a fresh-session reconnect.__ 'recvMessage' is delegated straight
  to the underlying client and a consumer loop spans reconnects. But a QoS 1\/2 message
  that was received-but-/unconsumed/ before the drop, then consumed after a
  /fresh/-session reconnect, makes the engine emit an acknowledgement carrying an
  old-session packet id on the new session — a stale ack the broker may ignore or
  reject. Consume promptly to avoid this; a full fix would require a core change.
-}
module Network.Mqtt.Client.AutoReconnect (
  -- * Configuration
  BackoffConfig (..),
  defaultBackoffConfig,
  AutoReconnectConfig (..),
  defaultAutoReconnectConfig,

  -- * Handle & status
  AutoClient,
  Status (..),
  underlying,

  -- * Lifecycle
  connect,
  withClient,
  disconnect,
  waitClosed,
  status,
  isConnected,

  -- * Messaging
  publish,
  publish_,
  subscribe,
  subscribe1,
  unsubscribe,
  ping,
  subscriptions,

  -- * Receiving
  recvMessage,
  tryRecvMessage,
  recvMessageSTM,
) where

import Network.Mqtt.Client.AutoReconnect.Internal
