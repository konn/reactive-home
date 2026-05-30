# haskesame-core

Low-level Sesame OS3/Sesame 5 protocol support for Haskell.

The package currently provides:

* Sesame OS3 BLE packet fragmentation/reassembly and message codecs.
* Sesame 5 mechanical status/settings decoders.
* Sesame OS3 session key derivation and AES-CCM packet encryption via `crypton`.
* A small transport abstraction.
* A low-level Sesame 5 client for login, lock, unlock, toggle, and mechanical settings.

BlueZ/D-Bus transport support lives in the `haskesame-transport-bluez` package.

## Prior work

`haskesame` is a Haskell implementation informed by the Python prior work
[`gomalock`](https://github.com/meronepy/gomalock) and its MQTT bridge
[`ssm2mqtt`](https://github.com/meronepy/ssm2mqtt).

## Copyright

2026-present (c) Hiromi ISHII
