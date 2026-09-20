# switchbot-bluez

Linux BlueZ D-Bus advertisement scanner for `switchbot-core`, parallel to
`switchbot-simpleble`. It needs a running system D-Bus, BlueZ, and a powered LE
adapter; it does not link SimpleBLE or open GATT connections.

Run `cabal run switchbot-scan-bluez` for a 20-second discovery scan. The library's
`scanSensors` accepts an optional adapter name (`hci0`), object path
(`/org/bluez/hci0`), or MAC address.

Finite discovery sessions use `Transport = le` and `DuplicateData = true`.
Only fresh manufacturer advertisements in the current window produce readings;
cached BlueZ device objects alone never refresh sensor timestamps. Partial
property updates are merged to recover service model data, and invalidated or
removed device properties are discarded. Stopping releases only this client's
discovery session. D-Bus calls have five-second deadlines.

Tests replay BlueZ signals and exercise discovery against a private D-Bus daemon
with a fake BlueZ service. Install `dbus-daemon` to run the latter tests; physical
Bluetooth hardware is not required for the test suite.
