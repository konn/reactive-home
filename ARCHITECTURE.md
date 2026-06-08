# reactive-home Architecture

This file is the root map for the repository's design documentation. It explains
how the package families fit together and points to the detailed architecture
notes under `ARCHITECTURES/`.

## Package Families

`reactive-home` is a Cabal monorepo. The packages currently fall into three
families:

- **MQTT (`hasquitto-*`)**: a low-level MQTT v5 client, automatic reconnect
  wrapper, and integration helpers. The mature core design is documented in
  `hasquitto-core/ARCHITECTURE.md`; the family index is
  `ARCHITECTURES/hasquitto.md`.
- **Sesame bridge (`haskesame-*`, `simpleble-hs`)**: a Sesame 5 BLE client,
  BlueZ/SimpleBLE transports, and MQTT bridge executables. The bridge design is
  documented in `ARCHITECTURES/sesame-bridge.md`.
- **FRP home automation (`reactive-home`)**: the Rhine/Effectful home automation
  layer. It currently consumes MQTT abstractions and remains early-stage
  scaffolding.
  Its ESPresense wrapper subscribes to configured device/sensor topics and
  processes them as deltas: per-sensor device updates/removals and room
  presence updates/removals. Full snapshots are aggregated only at the final
  stage. ESPresense sensor snapshots and room presence expire on a Rhine
  heartbeat clock in parallel with the Mackerel batching clock, so stale
  presence is removed even when no new MQTT report arrives.
  The app defaults to broker-assigned MQTT client identifiers; a stable
  `clientId` is an optional top-level config setting for deployments that need
  one.

## Dependency Direction

The intended direction is outward from low-level protocol libraries toward
applications:

1. Pure protocol and client packages define stable vocabularies and IO APIs.
2. Transport packages implement narrow transport records.
3. Bridge/application packages adapt concrete transports and configuration into
   long-running processes.
4. The FRP layer can consume the MQTT-facing surfaces without depending on BLE
   transport internals.

Design changes should preserve these boundaries unless the relevant architecture
document is updated with a new rationale.

## Architecture Documents

- `ARCHITECTURES/hasquitto.md` - MQTT package family map and pointer to the
  detailed `hasquitto-core` architecture.
- `ARCHITECTURES/sesame-bridge.md` - Sesame BLE client, transport, and MQTT
  bridge architecture.
- `hasquitto-core/ARCHITECTURE.md` - detailed hasquitto-core protocol/client
  design.

When changing package boundaries, concurrency models, protocol recovery, retry
semantics, topic/payload contracts, or public configuration surfaces, update the
matching architecture document in the same change.
