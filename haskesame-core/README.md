# haskesame-core

Low-level Sesame OS3/Sesame 5 protocol support for Haskell.

The package currently provides:

* Sesame OS3 BLE packet fragmentation/reassembly and message codecs.
* Sesame 5 mechanical status/settings decoders.
* Sesame OS3 session key derivation and AES-CCM packet encryption via `crypton`.
* A small transport abstraction plus a BlueZ D-Bus GATT transport.
* A low-level Sesame 5 client for login, lock, unlock, toggle, and mechanical settings.

The BlueZ layer expects object paths for the device, write characteristic, and
notify characteristic. The Sesame UUIDs from the upstream protocol are:

* service: `0000fd81-0000-1000-8000-00805f9b34fb`
* write characteristic: `16860002-a5ae-9856-b6d3-dbb4c676993e`
* notify characteristic: `16860003-a5ae-9856-b6d3-dbb4c676993e`

## Copyright

2026-present (c) Hiromi ISHII
