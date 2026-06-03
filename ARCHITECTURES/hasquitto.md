# Hasquitto Architecture

The Hasquitto packages provide the repository's MQTT client stack.

## Packages

- `hasquitto-core`: the low-level MQTT v5 protocol vocabulary, codec,
  connection abstraction, TCP backend, and concurrent client engine.
- `hasquitto-auto-reconnect`: an automatic reconnect layer over
  `hasquitto-core`.
- `hasquitto-effectful`: Effectful integration helpers.

## Detailed Design

The detailed architecture and rationale for the core MQTT client are maintained
in `hasquitto-core/ARCHITECTURE.md`. Read that document before changing the
codec, packet vocabulary, connection abstraction, client engine, flow control,
acknowledgement handling, or reconnect behavior.

This file exists so the repository-level `ARCHITECTURES/` directory has a
family-level entry for MQTT. If future changes add cross-package Hasquitto
design decisions that are not specific to `hasquitto-core`, document them here
and keep `hasquitto-core/ARCHITECTURE.md` focused on the core package.
