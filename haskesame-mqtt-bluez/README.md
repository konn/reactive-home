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
uuid = "12345678-90ab-cdef-1234-567890abcdef"
secret_key = "1234567890abcdef1234567890abcdef"
device_path = "/org/bluez/hci0/dev_XX_XX_XX_XX_XX_XX"
write_characteristic_path = "/org/bluez/hci0/dev_XX_XX_XX_XX_XX_XX/service000c/char000d"
notify_characteristic_path = "/org/bluez/hci0/dev_XX_XX_XX_XX_XX_XX/service000c/char000f"
manufacturer_data = "0500011234567890abcdef1234567890abcdef"
```

## Copyright

2026-present (c) Hiromi ISHII
