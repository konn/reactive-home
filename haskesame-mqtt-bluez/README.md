# haskesame-mqtt-bluez

Executable MQTT bridge for Sesame 5 over BlueZ/D-Bus.

Run with:

```sh
haskesame-mqtt-bluez --config /etc/haskesame-mqtt-bluez/config.toml
```

The app reads the specified TOML file. If `--config` is omitted, it reads `config.toml` from the current directory.

Example:

```toml
[mqtt]
host = "localhost"
port = 1883
user = ""
password = ""
client_id = "haskesame-mqtt-bluez"

[bridge]
base_topic = "ssm2mqtt"
history_name = "ssm2mqtt"

[[devices]]
mac_address = "XX:XX:XX:XX:XX:XX"
secret_key = "1234567890abcdef1234567890abcdef"
```

The app can discover the BlueZ device path, Sesame write/notify characteristics, manufacturer advertisement data, and Sesame UUID from `mac_address`. `uuid`, `device_path`, `write_characteristic_path`, `notify_characteristic_path`, and `manufacturer_data` may still be provided to override discovery.

## Copyright

2026-present (c) Hiromi ISHII
