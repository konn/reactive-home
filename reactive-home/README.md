# reactive-home

## SwitchBot BLE sensors and Hometrics

Hub 2, CO₂センサー (Meter Pro CO2), and 防水温湿度計 (Indoor/Outdoor Meter)
can supply temperature, humidity, CO₂, battery percentage and Hub 2 light level
where available. On Raspberry Pi/Linux, run `cabal run switchbot-scan-bluez` to
discover their stable device IDs without pairing. On macOS/SimpleBLE, use
`cabal run switchbot-scan`. Use [switchbot.example.toml](switchbot.example.toml) as a
starting point:

```toml
[hometrics]
endpoint = "http://localhost:8080/temperatures"

[switchbot.sensors]
room = "AA:BB:CC:DD:EE:01"
co2 = "AA:BB:CC:DD:EE:02"
outdoor = "AA:BB:CC:DD:EE:03"
```

Run `cabal run reactive-home -- --config reactive-home/switchbot.example.toml`
after replacing the example IDs. The Hometrics endpoint is a configurable full
URL, including its port and path; no cloud credentials are needed in
reactive-home. The local Hometrics server must be running. **CO₂ requires the
updated Hometrics server and Worker**; an older server silently ignores CO₂ in
mixed requests.

The Linux build selects BlueZ automatically and does **not** require SimpleBLE.
It needs a running system D-Bus/BlueZ service and a powered BLE adapter. Set
`adapter = "hci0"` under `[switchbot]` to select one explicitly. Other platforms
use SimpleBLE. To select SimpleBLE on Linux, build/run with `-fsimpleble`.

Each sensor can choose its **Hometrics device name and measurement fields**:

```toml
[switchbot.sensors]
hub2 = { id = "AA:BB:CC:DD:EE:01", hometrics_name = "living-room", hometrics_fields = ["temperature", "humidity"] }
co2 = { id = "AA:BB:CC:DD:EE:02", hometrics_name = "living-room", hometrics_fields = ["co2"] }
outdoor = { id = "AA:BB:CC:DD:EE:03", hometrics_name = "garden", hometrics_fields = ["temperature"] }
```

Here Hub 2 supplies living-room temperature/humidity, and the CO₂ sensor supplies
living-room CO₂. Names can be shared when selected fields do not overlap.
Omitting `hometrics_name` uses the configuration key; omitting `hometrics_fields`
sends all available temperature, humidity and CO₂ readings. Set
`hometrics_fields = []` to disable Hometrics
for that sensor. Rhine and MQTT still receive the complete sensor readings.
The supported field names are `temperature`, `humidity`, and `co2`.

Scanning defaults to a five-second window, reporting to once a minute and
freshness to two minutes. Override `scan_window`, `report_interval` or
`stale_after` under `[switchbot]`. Destination workers retain the latest pending
reading per sensor and retry failures without blocking scanning. Expired pending
readings are discarded. This is current-state sampling, not a durable history
queue.

Enable MQTT relaying **per sensor** by replacing its ID string with an inline
table containing `id` and `mqtt_topic`. Put the shared broker `host` and optional
`port` before any table headers:

```toml
host = "localhost"
port = 1883

[hometrics]
endpoint = "http://localhost:8080/temperatures"

[switchbot.sensors]
hub2 = "AA:BB:CC:DD:EE:01"
co2 = { id = "AA:BB:CC:DD:EE:02", mqtt_topic = "switchbot/co2/state" }
outdoor = { id = "AA:BB:CC:DD:EE:03", mqtt_topic = "garden/climate" }
```

This sends all three sensors to Hometrics and relays only `co2` and `outdoor`
to their exact MQTT topics. Omitting `mqtt_topic` disables MQTT delivery for
that sensor; the plain ID string remains the shortest form. Add the same
`hometrics_name = "...", hometrics_fields = [...]` options alongside `mqtt_topic`
to combine both destinations. JSON messages
include observation timestamps and use QoS 1 without retention. Remove
`[hometrics]` to disable Hometrics delivery. BLE with Hometrics alone needs no
MQTT broker.

For custom reactive processing, compose `Home.Reactive.SwitchBot.switchBotS`
with your Rhine signal functions. It yields newly observed named samples and a
snapshot that expires inactive sensors, even on empty scan ticks. See the
[sensor architecture](../ARCHITECTURES/switchbot.md) for APIs and wire formats.

## Configuration

`clientId` is optional. When omitted, `reactive-home` asks the MQTT broker to
assign a unique client identifier, which avoids client-id takeover conflicts if a
previous interrupted process is still disconnecting or reconnecting. Set
`clientId = "..."` only when a stable MQTT client identity is required.

### Scheduled MQTT switches

Declare recurring switches under `mqtt.scheduled_switches`:

```toml
[mqtt]

[[mqtt.scheduled_switches]]
name = "Homepod Bump"
topic = "delay/homepod-bump/state"
interval = "5m"
on_duration = "10s"
```

Each switch publishes `false` when its scheduler starts, then `true` after the
first `interval` and `false` after `on_duration`. Activations repeat every
`interval`, measured from scheduler startup: this example turns on at 5, 10,
15, ... minutes, for 10 seconds each time. Multiple switches run independently.
Both states are retained MQTT messages published at QoS 1, so new subscribers
receive the latest state. Scheduled switches also appear by `name` in the MQTT
switch snapshot and can be used in unlock/autolock dismissal conditions.

Durations accept the existing `ms`, `s`, `m`, `h`, and `d` suffixes (or bare
seconds). Both durations must be finite and positive, and `on_duration` must be
shorter than `interval`. Scheduling uses the app's 500 ms heartbeat and does not
require incoming MQTT traffic or any other configured devices. Deadlines are
handled on the first heartbeat at or after they expire. A delayed activation
still gets its full on duration after publishing completes; missed activations
are skipped without replaying a burst of old pulses. Durations or OFF gaps
shorter than 500 ms can be stretched or skip activations. Restarting the app
resets the schedule.

## Copyright

(c) Hiromi ISHII 2026- present
