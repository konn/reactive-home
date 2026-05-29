# haskesame-transport-bluez

BlueZ/D-Bus transport implementation for `haskesame-core`.

This package provides `Network.Sesame.Transport.Bluez`, which connects the
core Sesame transport abstraction to BlueZ GATT characteristic object paths.

The Sesame UUIDs from the upstream protocol are:

* service: `0000fd81-0000-1000-8000-00805f9b34fb`
* write characteristic: `16860002-a5ae-9856-b6d3-dbb4c676993e`
* notify characteristic: `16860003-a5ae-9856-b6d3-dbb4c676993e`

## Copyright

2026-present (c) Hiromi ISHII
