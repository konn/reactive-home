# hasquitto-core — Architecture

This document explains the *why* behind `hasquitto-core`: the cross-cutting design
and the architectural decisions a contributor should understand before touching
the codec or the client engine. Per-module behaviour is documented in the module
Haddock; this complements it with rationale.

## Purpose & goals

`hasquitto-core` is a from-scratch, **low-level MQTT v5 client** library. The goal
is a clean, consistent, well-abstracted API.
 The library exposes both a high-level client and the pure building blocks beneath it,
so it can also serve as a toolkit for unusual transports or bespoke MQTT tooling.

## Guiding principles

- **No streaming-library dependency.** No `conduit`, `streaming`, `pipes`, or
  `streamly`. I/O is expressed with plain `IO` actions and a pull-based receive
  API, in the spirit of `http-client`'s `BodyReader`.
- **Layered after `http-types` / `http-client` / `streaming-commons`.** A pure
  protocol-vocabulary layer (like `http-types`), a `Connection` abstraction with a
  pluggable transport and pull-based reads (like `http-client`), and a thin
  `runTCPClient` / `clientSettings` socket layer (like `streaming-commons`'
  `Data.Streaming.Network`, but built directly on `network`).
- **Namespace:** everything lives under `Network.Mqtt.*` (camelised `Mqtt`).
- **Record conventions:** `NoFieldSelectors` + `OverloadedRecordDot` +
  `DuplicateRecordFields` (+ `BlockArguments`). Fields are bare nouns (`topic`,
  `qos`, `properties`); access is `x.field`. Validated types use `mk*` smart
  constructors; option records use `default*` values you override.
- **`*.Internal` modules are hidden.** `cabal-gild` auto-discovers modules:
  anything matching `**/Internal.hs` or `**/Internal/**` becomes a hidden
  `other-module`; everything else is exposed. Public modules are curated
  re-exports over the internals.

## Layered architecture

Four layers, each depending only on those above it. Layers (a)+(b) are pure and
IO-free; (c)+(d) introduce IO and concurrency.

| Layer | Modules | Role |
|-------|---------|------|
| **(a) Vocabulary** (pure) | `Network.Mqtt.Types` and `Network.Mqtt.Types.{QoS,Topic,Property,ReasonCode,PacketId,Will,Packet}` | The MQTT v5 protocol types: QoS, topics/filters, properties, reason codes, packet identifiers, the `Packet` sum + per-packet records. |
| **(b) Wire codec** (pure) | `Network.Mqtt.Codec`; `Network.Mqtt.Codec.{Parser,Encode,Decode,Wire}.Internal` | Encode a `Packet` to a `Builder`/`ByteString` and decode bytes to a `Packet`, with full wire-format validation. |
| **(c) Connection** (IO) | `Network.Mqtt.Connection`, `Network.Mqtt.Connection.TCP`, `Network.Mqtt.Connection.Internal` | A swappable byte transport (`Connection`), a buffered handle (`Conn`), packet framing (`readPacket`/`writePacket`), and a plain-TCP backend. |
| **(d) Client** (IO) | `Network.Mqtt.Client`, `Network.Mqtt.Client.Internal`, `Network.Mqtt.Message`, `Network.Mqtt.Exception` | The concurrent client engine and its public API; the received-message type and the exception hierarchy. |

**A deliberate split point.** Layers (a)+(b) are pure and import only
`base`/`bytestring`/`text`. They could later move into a `hasquitto-types` package
(mirroring the `http-types` / `http-client` division) by relocating the
directories and adding a dependency — with zero import-path churn, because the
module names are stable. `-Wunused-packages` guards the boundary: an accidental
IO dependency creeping into the vocabulary layer breaks the build.

## Key decisions & rationale

### The framing insight (why no streaming library)

Every MQTT control packet on the wire is exactly
`[1 header byte][1–4-byte Remaining-Length VBI][exactly N body bytes]`. The
Remaining Length gives the body size *before* any structure is parsed. So the
socket layer reads one **complete** frame's bytes (header → byte-by-byte VBI →
exactly N body bytes), then runs a **pure, total** decoder over a fully
materialised strict `ByteString`. No incremental/streaming parser ever crosses
the socket — "ran out of input" is a hard `DecodeError`, never a request for more
bytes. This is precisely what lets the package avoid every streaming library.

### Hand-rolled `Parser`, not `binary`

The decoder is a small total parser
(`newtype Parser a = Parser (ByteString -> Either DecodeError (a, ByteString))`).
Versus depending on `binary`'s `Get`: it yields typed `DecodeError`s (not
`String`), is genuinely total over a complete buffer, operates directly on the
strict `ByteString` that `recv` returns, and keeps the dependency footprint
minimal under `-Wunused-packages`. `binary` would not remove the hard parts (the
fields are dependent on packet type, flags, and property legality).

### Total faithful encoder + validating decoder

`encodePacket :: Packet -> Builder` is **total and faithful** — a deliberate
low-level escape hatch (as `http-types` lets you build any `Status`). Validity is
enforced where it matters instead:

- **Smart constructors** on vocabulary types (`mkTopic`, and packet ids that are
  pool-allocated and nonzero).
- **The decoder** validates the wire rules: fixed-header flags per packet type,
  per-packet **property legality** and value ranges, per-packet **reason-code**
  sets, minimal Variable-Byte-Integer encoding, and the compact acknowledgement
  forms (PUBACK/PUBREC/PUBREL/PUBCOMP/DISCONNECT/AUTH).

The one place the encoder refuses bad input is length prefixes: `putText`/
`putBytes` raise an error rather than silently truncating a value over 65535 bytes
(which would emit a corrupt packet).

### Distinct types for safety

- `Topic` (publish names) and `TopicFilter` (subscribe filters with `+`/`#`) are
  separate newtypes with separate smart constructors.
- `ReasonCode` is a `Word8` newtype plus pattern synonyms: a forward-compatible
  representation, while the decoder still enforces the spec's closed per-packet
  set so unknown codes are rejected at the wire boundary.

### Connection abstraction

`Connection` is a raw record of `connectionRead`/`connectionWrite`/
`connectionClose` (empty read = EOF, the `http-client` contract). `Conn` wraps it
with a leftover/pushback buffer for framing. `readPacket`/`writePacket` are the
framing API. The transport is pluggable: the TCP backend
(`Network.Mqtt.Connection.TCP`, the only module importing `network`) is one
implementation; TLS or WebSocket backends produce the same `Conn` — the
`http-client` / `http-client-tls` swap pattern.

### Client concurrency model

- **Synchronous handshake first.** `connect` establishes the transport and runs
  the CONNECT/AUTH/CONNACK exchange *on the calling thread* (no reader thread
  exists yet), bounded by an STM `registerDelay` timeout.
- **Then background threads, via `async`** (not `withAsync`, because `connect`
  returns the live `Client`): a **reader** (sole socket reader; dispatches
  packets), a **keepalive** thread (sends PINGREQ when idle, detects an overdue
  PINGRESP), and an **ack-writer** worker (see backpressure below). Their `Async`
  handles live in `Client` and are cancelled on `disconnect`/`reconnect`.
- **One write lock.** All sends funnel through an `MVar` held across the `sendAll`
  syscall, so bytes never interleave — an `MVar`, not STM, because the lock spans
  arbitrary IO.
- **Request correlation.** SUBACK/UNSUBACK/PUBACK/PUBREC/PUBCOMP are matched to
  waiting callers through transactional `stm-containers` maps keyed by packet id
  (`pending`, `outInflight`, `inQoS2`). Per-key transactions avoid the whole-map
  contention a single `TVar (Map ...)` would impose.
- **Packet-id pool.** Ids run 1..65535 (0 is illegal), wrap, skip in-use, and
  `retry` (block) when exhausted — natural backpressure.
- **Lifecycle without `link`.** Reader/keepalive failures are recorded in a
  `closed :: TMVar CloseReason`; API operations observe it and fail cleanly, and
  `waitClosed` reads it. Linking would throw async exceptions into whatever thread
  happened to call `connect` — surprising and avoided. Request flows are
  `mask`/`finally`-guarded so an async exception cannot leak a packet id or orphan
  a waiter, and waits use `takeTMVar \`orElse\` readTMVar closed` so a waiter
  registered *after* a close still wakes (no lost-wakeup window).

### Pull-based reception

Messages are consumed with `recvMessage` / `tryRecvMessage` / `recvMessageSTM`
(STM-composable), backed by a bounded `TBQueue` — not callbacks. This gives
backpressure and composability, and a slow consumer can never stall protocol acks
or keepalive (the callback footgun). A callback loop is trivially recoverable as
`forever (recvMessage c >>= cb)`; the reverse is the boilerplate we remove.

### Acknowledge-on-consume backpressure

The sole reader thread must never block on application delivery, or it would stall
PINGRESP/PUBREL/ack processing. So for inbound QoS 1/2, **the ack is sent when the
message is consumed**, not on receipt: the inbound message holds its
Receive-Maximum quota slot until consumed, the broker therefore never exceeds
`Receive Maximum` in flight, and — with the queue sized ≥ Receive Maximum — the
reader's enqueue can never block. Because the ack is a socket write but
consumption can happen inside STM (`recvMessageSTM`), consuming atomically posts a
pending-ack command to an STM queue that the **ack-writer** worker drains and
sends; STM never performs IO. QoS 0 has no flow control, so a full queue follows a
configurable `OverflowPolicy` (`DropNewest` | `DropOldest` | `Block`).

### QoS 1/2 state machines

Outbound — QoS 1: PUBLISH → await PUBACK; QoS 2: PUBLISH → PUBREC → PUBREL → await
PUBCOMP (an error PUBREC short-circuits without sending PUBREL). Inbound QoS 2 is a
proper per-id state machine (`AwaitingPubRel` → `AwaitingConsume`): a duplicate
PUBLISH re-sends PUBREC without re-delivering; a PUBREL for an unknown id is
answered with PUBCOMP reason `0x92` (Packet Identifier Not Found), so no tombstone
is needed. **Orphan ack self-completion**: an ack with no waiting caller — e.g. a
publish replayed after reconnect whose original caller has gone — still drives the
handshake to completion and frees the id (a failed orphan PUBREC ends the exchange
without PUBREL).

### Flow control & capability gates

Negotiated CONNACK capabilities are cached and enforced on later calls: a
**Receive-Maximum** send quota bounds concurrent unacked QoS>0 publishes;
**Maximum Packet Size** is enforced in both directions; and capability gates
reject what the server disallows (publishing above **Maximum QoS** is *rejected*,
never silently downgraded; retain when unavailable; wildcard / subscription-id /
shared subscriptions when unsupported), honour a **Server Keep Alive** override,
and surface an **Assigned Client Identifier**.

### Bidirectional Topic Alias

Inbound aliases are resolved against an alias→topic map (with the protocol-error
checks the spec requires, including rejecting an empty topic name for an
unestablished alias). Outbound aliasing assigns and reuses aliases up to the
server-advertised maximum; crucially, alias lookup-or-assign, packet construction,
the map update, and the socket write all happen inside the **single write-lock
critical section**, so two concurrent publishes to the same topic can never send
an alias-only packet before its establishing packet.

### Enhanced authentication

When an `Authenticator` is configured, CONNECT carries the Authentication Method,
and the synchronous handshake loop answers each server AUTH `0x18` (Continue
authentication) challenge. Mid-session `reauthenticate` sends AUTH `0x19` and runs
a serialized state machine **on the caller's thread**; the reader merely hands off
every incoming AUTH packet (continue, terminal success, and failures) via a
`TMVar`, so an arbitrary-IO auth callback never runs on — and stalls — the reader.
The Authentication Method is checked for consistency across the exchange.

### Session & reconnect groundwork

In-flight QoS 1/2 state is owned by the `Client`. `reconnect` re-runs the
handshake on a fresh connection while holding the write lock; on
`sessionPresent = True` it replays surviving outbound state (PUBLISH with DUP=1,
re-issued PUBREL), and on a fresh session it discards in-flight state and resets
the id pool. In-flight ids stay reserved (they are freed only on completion), so
replay does not collide with new requests. An automatic reconnect *driver* is left
as future work; `reconnect` is the manual building block.

## Error model

A single `MqttException` wraps `DecodeError` (pure, from the parser, lifted to
`throwIO` only at the `readPacket` IO boundary), `ProtocolError` (CONNACK refusal,
server DISCONNECT, keep-alive timeout, oversize packet, auth failure, unsupported
capability, …), and `ConnectionError` (clean EOF vs truncated mid-frame). Timeouts
and close-aware waits compose via STM `registerDelay` / `orElse`, avoiding races
between "timed out", "result arrived", and "connection closed".

## Testing strategy

- **Unit / property** (`test/Test.hs`, tasty + falsify): a round-trip property —
  `decodeFrame . encodePacketBS == id` — over generators for all 15 packet types,
  plus negative golden cases (wrong fixed-header flags, non-minimal VBI, packet id
  0, illegal property/reason placement, compact ack forms) and topic-match cases.
- **Integration** (`test-integration/Integration.hs`, tasty + tasty-hunit):
  connect handshake, server-assigned client id, publish QoS 0/1/2 results, and
  subscribe → publish → `recvMessage` round-trips against a real broker. The
  endpoint is configurable via `--mqtt-host` / `--mqtt-port` (default
  `localhost:1883`, also `TASTY_MQTT_HOST` / `TASTY_MQTT_PORT`). CI runs it against
  an `eclipse-mosquitto:2.0.18` service container (`.github/workflows/haskell.yml`).

## Known limitations / future work

- No automatic reconnect driver yet (manual `reconnect` only).
- Only a plain-TCP transport ships; TLS and WebSocket `Connection` backends are
  the obvious next backends (the abstraction already supports them).
- The pure layers could be split into a separate `hasquitto-types` package.
- The TCP backend connects to the first resolved address only; multi-address
  fallback (try each `getAddrInfo` result until one connects) would be more robust
  on dual-stack hosts.
