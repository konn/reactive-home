# haskesame-mqtt-simpleble

Executable MQTT bridge for Sesame 5 over SimpleBLE.

Run with:

```sh
haskesame-mqtt-simpleble --config /etc/haskesame-mqtt-simpleble/config.toml
```

Example:

```toml
[mqtt]
host = "localhost"
port = 1883
user = ""
password = ""
client_id = "haskesame-mqtt-simpleble"

[bridge]
base_topic = "ssm2mqtt"
history_name = "ssm2mqtt"
debug_logging = true

[[devices]]
mac_address = "XX:XX:XX:XX:XX:XX"
uuid = "00000000-0000-0000-0000-000000000000"
secret_key = "1234567890abcdef1234567890abcdef"
```

`mac_address` and `uuid` are both optional, but at least one must be supplied. On macOS, SimpleBLE may expose a CoreBluetooth identifier instead of the real BLE MAC address, so `uuid` is the preferred selector there. `service_uuid`, `write_characteristic_uuid`, `notify_characteristic_uuid`, and `scan_timeout_ms` may be supplied as optional overrides.

## Copyright

2026-present (c) Hiromi ISHII
