# Sesame Bridge Architecture

This document describes the current architecture and rationale for the Sesame 5
BLE to MQTT bridge. It covers the `haskesame-*` packages and the small
`simpleble-hs` binding package.

## Package Layout

- `haskesame-core`: Sesame protocol types, codec, crypto, transport record, and
  the `Sesame5Client`.
- `haskesame-transport-bluez`: Linux BlueZ D-Bus implementation of
  `SesameTransport`.
- `haskesame-transport-simpleble`: SimpleBLE implementation of
  `SesameTransport`.
- `simpleble-hs`: Haskell bindings used by the SimpleBLE transport.
- `haskesame-mqtt`: transport-agnostic MQTT bridge logic and MQTT topic/payload
  mapping.
- `haskesame-mqtt-bluez`: TOML-configured BlueZ bridge executable.
- `haskesame-mqtt-simpleble`: TOML-configured SimpleBLE bridge executable.

The core bridge does not know about BlueZ or SimpleBLE. Executable packages
adapt configuration into a `BridgeDevice`, whose `connectSesameClient` action
returns a live `ConnectedBridgeDevice`.

## Layering

The Sesame stack has four layers:

| Layer | Packages/modules | Role |
| --- | --- | --- |
| Protocol/client | `haskesame-core`, `Network.Sesame.*` | Sesame message vocabulary, wire codec, crypto, command API, and publish/response demultiplexing. |
| BLE transport | `Network.Sesame.Transport`, `haskesame-transport-*` | Narrow transport record: send BLE fragments, receive BLE messages, close the session, and expose advertisement data. |
| MQTT bridge | `haskesame-mqtt`, `Network.Sesame.Mqtt.*` | Command subscription, retained status publication, command queueing, confirmation, requeue, and reconnect policy. |
| Apps | `haskesame-mqtt-bluez`, `haskesame-mqtt-simpleble` | TOML config, MQTT client setup, concrete BLE transport selection, Sesame login, and process entry points. |

The dependency direction is one-way: apps depend on the bridge and a concrete
transport; the bridge depends only on `haskesame-core` and MQTT; transports
depend on `haskesame-core`; the core package depends on no bridge/app code.

## Protocol Client

`Sesame5Client` owns:

- a `SesameTransport`;
- the current optional cipher;
- response waiters keyed by Sesame item code;
- a publish queue.

`newSesame5ClientWith` starts one receive loop. The receive loop is deliberately
not linked into the caller. Transport and protocol errors are broadcast into the
response waiters and publish queue, where normal API operations observe them.
This keeps failures local to the client session instead of throwing asynchronous
exceptions into the bridge supervisor.

Login is synchronous:

1. Wait for the unencrypted `Initial` publish and session token.
2. Derive the session key from the configured secret.
3. Install the cipher.
4. Send the encrypted login command.
5. Wait for both initial `MechStatus` and `MechSetting` publishes, then push
   them back into the publish queue for the bridge status loop.

Commands (`lock`, `unlock`, `toggle`, and settings updates) are encrypted after
login and wait for their matching response using bounded timeouts.

## Transport Contract

`SesameTransport` is intentionally small:

```haskell
data SesameTransport = SesameTransport
  { sendBle :: Bool -> ByteString -> IO (Either SesameTransportError ())
  , receiveBle :: IO (Either SesameTransportError (ByteString, Bool))
  , closeBle :: IO ()
  , advertisement :: IO (Either SesameTransportError Advertisement)
  }
```

There is one close path. Timed-out or failed bridge sessions are closed with
`closeBle` and then reconnected by the bridge supervisor. There is no hard abort
operation; the earlier abort path was removed after the runtime issue was traced
to two bridges contending for the same BlueZ device.

The BlueZ transport resolves device, write characteristic, notify
characteristic, and advertisement data from D-Bus managed objects. It starts
notifications before returning a transport, snapshots the notify characteristic
value to avoid missing the initial token, and logs `BlueZ transport ready` once
the transport can feed the Sesame login.

BlueZ may report a device as connected even when the session is stale or services
are unresolved. The transport therefore:

- reuses a connected session only when services are already resolved;
- waits for services if the device is connected but incomplete;
- disconnects stale sessions before a new connect attempt when needed;
- bounds D-Bus calls with explicit timeouts;
- performs normal `StopNotify`, `Device1.Disconnect`, and D-Bus client close on
  `closeBle`.

The SimpleBLE transport implements the same record for systems where SimpleBLE is
the preferred BLE backend.

## MQTT Contract

The MQTT bridge exposes an `ssm2mqtt`-compatible topic shape:

- commands: `<base>/<uuid>/set`
- retained status: `<base>/<uuid>/get`

The default base topic remains `ssm2mqtt`; deployments can configure it, and the
current BlueZ deployment uses `haskesame`.

Accepted command payloads are UTF-8 text:

- `LOCKED` -> `CommandLock`
- `UNLOCKED` -> `CommandUnlock`

Status is retained JSON derived from Sesame `MechStatus`:

- `position`
- `lockCurrentState`
- `batteryVoltage`
- `batteryLevel`
- `chargingState`
- `statusLowBattery`

The bridge publishes status with QoS 1 and retained delivery so Home Assistant or
other MQTT consumers can reconstruct current state after restart.

## Bridge Concurrency Model

`runBridge` subscribes to the command topic filter first, logs subscription
readiness, then runs:

- one command consumer reading MQTT messages;
- one supervised device session loop per configured Sesame device.

Per device, the bridge maintains:

- a connected flag;
- latest status version and snapshot;
- last BLE/status activity;
- last command activity;
- at most one pending command.

The pending-command map is keyed by UUID and stores only the latest command. This
coalesces repeated lock/unlock requests and prevents an old command backlog from
executing after reconnect.

QoS behavior is intentional:

- QoS 0 commands are ignored while the device may be unavailable.
- QoS 1 and QoS 2 commands are retained in the bridge while the Sesame session is
  reconnecting.
- Commands that arrive while disconnected are marked `force_send=True`, so they
  are sent after reconnect even if the last known snapshot already appears to
  satisfy the command.

When connected, the command loop skips a command if the current status snapshot
already satisfies it and the command was not forced. Otherwise it sends the
Sesame command and waits for a confirming status version whose snapshot matches
the target state.

## Command Confirmation and Recovery

The bridge treats a command as successful when either:

- the post-command Sesame status arrives before the command response; or
- the command response returns and a later status update confirms the target
  state.

The wait windows are deliberately bounded:

- short command response/status race;
- short late response/status grace;
- bounded post-command status wait.

If confirmation times out, the bridge requeues the command, closes the session
through the normal disconnect path, waits a short reconnect settle period, logs
the reconnect, and logs in again. If a newer pending command arrived meanwhile,
the newer command wins and the failed older command is not restored.

If the status loop terminates while a command is in flight, that command is
requeued before reconnect. Transport-closed exceptions are treated as session
closure rather than generic command failure.

## Reconnect Policy

The session loop is the supervisor. Connection failures back off with jitter and
a small cap, with a wider minimum delay for BlueZ local connection aborts. Normal
session termination is followed by either:

- a short command reconnect settle when a command is pending; or
- a passive reconnect settle when no command is pending.

The passive path keeps the bridge available after a disconnect without spinning
against BlueZ. The command path prioritizes pending user commands while still
giving BlueZ a brief period to settle.

## Readiness and Runtime Diagnostics

Debug logging is part of the operational model. Current readiness milestones are:

- `MQTT command subscription ready`
- `BlueZ transport ready`
- `Sesame login complete`
- `Sesame connected`
- `first Sesame status publish ready`

These markers separate MQTT readiness, BlueZ connect/service discovery,
notification setup, Sesame authentication, and MQTT state publication. They were
added so first-command latency can be attributed without adding active protocol
probing.

The observed healthy runtime shape is:

- cold BlueZ connection/login may take several seconds;
- after first status publication, command dispatch starts within milliseconds of
  MQTT receipt;
- status confirmation normally arrives in roughly one to two seconds;
- repeated commands that are already satisfied are skipped from the latest known
  status snapshot.

## Rationale

The bridge is intentionally conservative:

- It keeps a single active bridge per Sesame/BlueZ device. Running another
  bridge against the same device can corrupt or desynchronize the BLE session.
- It prefers normal close/reconnect over hard aborts. BlueZ state can be slow,
  but detached abort/disconnect recovery added risk and was only useful while
  debugging dual-bridge contention.
- It confirms commands by observing device status, not merely command responses.
  This gives MQTT consumers a retained state that reflects the lock's reported
  mechanical state.
- It stores only the latest pending command because lock control is stateful; the
  final desired state matters more than replaying every intermediate request.
- It avoids active ping/status probing for now. The runtime logs show reliable
  steady-state behavior, and passive status plus command-confirmation paths are
  enough for the current bridge.

Update this document when changing Sesame transport semantics, login flow,
MQTT topic/payload contracts, command queueing, confirmation logic, reconnect
policy, or operational readiness markers.
