# SwitchBot BLE Sensors

## Packages and data flow

`switchbot-core` contains the pure advertisement vocabulary and decoder in
`Network.SwitchBot.Advertisement`. It depends on no Bluetooth runtime. The
`switchbot-simpleble` adapter reuses `simpleble-hs`, the binding already used by
the Sesame transport. `switchbot-bluez` supplies a separate Linux D-Bus adapter,
following the same core/transport split as `haskesame-*`. Both expose the same
finite-window `scanSensors` entry point and read advertisements without pairing,
connecting, writing GATT characteristics, or involving the SwitchBot cloud.

`reactive-home` selects BlueZ on Linux, including Raspberry Pi, and SimpleBLE on
other platforms at build time. Its Linux dependency closure contains no
SimpleBLE package or native library. Cabal's `simpleble` flag explicitly selects
SimpleBLE on Linux when desired. CI builds both routes and checks each transitive
dependency closure with `ci/scripts/check-reactive-home-ble-plan.py`.

`Home.Reactive.SwitchBot` maps configured device IDs to sensor names and exposes
`switchBotS`, a Rhine signal function returning new samples and the current
sensor snapshot. `Home.Reactive.Sensor.sensorSnapshotS` expires old samples even
on empty input ticks. The injectable `SwitchBotClock` can run a live scanner or
an advertisement replay source. Measurements remain available for downstream
Rhine automations independently of reporting destinations.

The application starts this sensor network alongside the existing MQTT network,
including while the MQTT connection is being established. MQTT-only processing
does not start a BLE scanner. A sensor-only deployment needs no MQTT broker.

## Observed wire formats

The initial macOS capture on 2026-09-21 identified the requested devices by the
`fd3d` service data model byte, masked with `0x7f`:

| Device | Model | Minimum service bytes | Minimum manufacturer bytes | Temperature/humidity offset |
| --- | --- | --- | --- | --- |
| Hub 2 | `0x76` (`v`) | 2 | 16 | 13 |
| Meter Pro CO2 / CO₂センサー | `0x35` (`5`) | 3 | 15 | 8 |
| Indoor/Outdoor Meter / 防水温湿度計 | `0x77` (`w`) | 3 | 11 | 8 |

Offsets exclude the company ID (`0x0969`) and service UUID bytes. The first six
manufacturer bytes supply the device MAC, so identity is stable across macOS
CoreBluetooth UUIDs and Linux addresses. Configuration normalizes MAC addresses
to twelve uppercase hex digits. Supported alternate model codes are `V`, `0x15`
and `W`, respectively; unknown products are ignored.

Temperature has a decimal nibble followed by seven integer bits and a sign bit
(set means positive). Humidity is seven bits; its high bit indicates display
units, and does not change the Celsius wire value. Battery percentage is the
third service byte for the meters. CO₂ is the big-endian word at manufacturer
offset 13. Hub 2 light level is the low five bits at offset 12; it is a device
level, not lux.

The captured packets are regression fixtures with only their MAC bytes replaced.
Their original measurements were Hub 2: 25.0 °C / 73%; CO₂ meter: 25.3 °C /
66% / 584 ppm; outdoor meter: 21.8 °C / 96%. Reference layouts were checked
against [SwitchBot's BLE documentation](https://github.com/OpenWonderLabs/SwitchBotAPI-BLE/blob/latest/devicetypes/meter.md)
and the [pySwitchbot sensor parsers](https://github.com/sblibs/pySwitchbot/tree/57545dc988b5008ff8c10069e3c799baebe5ee3f/switchbot/adv_parsers).

All indexing follows length checks. Invalid decimals, unavailable temperature,
humidity/battery above 100%, and CO₂ warmup/overflow values are represented by
`Nothing`, preserving other valid measurements. CO₂ is accepted from 1 through
9999 ppm. A Hub 2 reporting both zero temperature and zero humidity is treated
as having no probe reading. Trailing bytes are tolerated.

## Scanning and recovery

One scanner owns finite scan windows, defaulting to five seconds. SimpleBLE
clears the seen-peripheral list at scan start; each window yields the latest
advertisement from each peripheral seen during that window. Samples are stamped
at the window end, so timestamps have scan-window resolution rather than exact
radio-arrival resolution. An empty window still ticks Rhine and expires stale
snapshots. Adapter failure logs an error, waits one window and retries adapter
selection; per-peripheral decoding errors do not discard other devices.

BlueZ opens a dedicated system-bus client per finite window and selects a powered
adapter by `hci0`-style name, object path, or MAC address. Its discovery filter
uses LE transport and repeated advertisement notifications. It merges
`InterfacesAdded` and `PropertiesChanged` data, but only manufacturer data
received during that window makes a measurement fresh. A cached device object,
RSSI change, or service-only update does not refresh stale measurements.
Invalidated properties and removed devices discard pending readings. D-Bus
calls have five-second deadlines; cancellation and normal completion release
only the scanner client's discovery session. See the
[BlueZ adapter API](https://bluez.readthedocs.io/en/latest/adapter-api/) for the
discovery filter and session contract. Hardware-independent tests replay signals
and run the scanner against a private D-Bus service.

Each SimpleBLE process should own its scanner adapter. SimpleBLE's adapter scan controls
are shared: callers embedding this scanner alongside another scanner in the
same process must coordinate scanning. Existing Sesame GATT sessions are not
modified by this feature.

The SimpleBLE binding's list enumeration now handles zero-length adapter,
peripheral, service, characteristic and manufacturer lists without unsigned
underflow. This is required for ordinary connectionless advertisements, whose
services contain no characteristics.

## Configuration and delivery

The concise sensor mapping lives in `[switchbot.sensors]`; names use ASCII
letters, digits, `_` and `-`, and each MAC may appear once. Each value is a MAC
string (default Hometrics projection, MQTT relay disabled), or an inline table
with `id` and optional `mqtt_topic`, `hometrics_name`, and `hometrics_fields`. A
`[switchbot.sensors.<name>]` subtable accepts the same options.
For example:

```toml
[switchbot.sensors]
hub2 = "AA:BB:CC:DD:EE:01"
co2 = { id = "AA:BB:CC:DD:EE:02", mqtt_topic = "home/air/quality", hometrics_name = "living-room", hometrics_fields = ["co2"] }
outdoor = { id = "AA:BB:CC:DD:EE:03", mqtt_topic = "garden/climate", hometrics_name = "garden", hometrics_fields = ["temperature", "humidity"] }
```

`mqtt_topic` is the exact publish topic; no prefix or suffix is added. Empty
topics and wildcards are rejected. Hometrics options are independent of MQTT
selection. Optional
`scan_window`, `report_interval` and `stale_after` default to `5s`, `60s` and
`2m`. Durations must be finite and positive; scan windows must be 100 ms through
60 seconds. Expiry must exceed the scan and reporting intervals.

`Home.Reactive.SwitchBot.Runtime` maintains a separate latest-pending map for
each enabled destination. Only sensors declaring `mqtt_topic` enter the MQTT
queue; sensors with at least one selected Hometrics field enter its queue when
the endpoint is set.
Successful delivery acknowledges only the versions
sent, preserving updates received during I/O. Failed deliveries stay pending and
are replaced by newer readings. Each reporting attempt filters out expired
samples. Memory is bounded by the number of configured sensors. These queues
are in memory, and are intended for periodic current-state metrics rather than
lossless historical telemetry. Retries can duplicate an ambiguously acknowledged
request. Restarting discards pending samples and starts fresh scans.

`[hometrics].endpoint` is the full configurable URL of the local Hometrics
REST endpoint. Each sensor can set `hometrics_name` to its Hometrics device name
and `hometrics_fields` to a subset of `temperature`, `humidity`, and `co2`.
The defaults are the sensor's configuration key and all three fields; only
available measurements are sent. An empty field list disables Hometrics for
that sensor. Empty names and unknown fields are rejected. Two sensors may share
a Hometrics name when their selected fields are disjoint; overlapping
name/field destinations are rejected to prevent silent overwrites. These
projections leave Rhine names, complete readings, and MQTT messages unchanged.

Requests are JSON objects with optional `temperatures`, `humidity` and `co2`
maps keyed by each sensor's Hometrics name. The `temperature` selection maps to
the REST API's plural `temperatures` key. Empty maps are omitted and empty
requests are skipped. Success requires HTTP 204; redirects and other statuses
fail the attempt. HTTP requests have a ten-second response timeout; delivery attempts also have
a ten-second overall deadline, including waiting for MQTT connectivity. Hometrics
owns cloud authentication, cloud batching and dataset routing. The REST API
currently assigns ingestion timestamps downstream; the BLE observation timestamp
is retained in Rhine/MQTT but cannot be supplied to this REST schema.

CO₂ requires the coordinated Hometrics native-server and Worker updates. Older
native servers ignore unknown JSON fields in mixed requests, so both components
must be upgraded before enabling CO₂ reporting. See the Hometrics repository
for its deployment procedure; this implementation does not deploy it.

Setting `mqtt_topic` on a sensor enables its relay using the application's
existing broker settings and connection. Sensors without the option never
publish to MQTT. Broker startup failures are retried every five seconds while
the sensor network continues running. Each selected sensor publishes to its
configured topic as QoS 1, non-retained JSON:

```json
{"sensor":"room","observedAt":"2026-09-21T00:00:00Z","reading":{"deviceId":"AABBCCDDEEFF","model":"MeterProCO2","temperatureC":25.3,"humidityPercent":66,"co2Ppm":584,"batteryPercent":100,"lightLevel":null}}
```

The timestamp lets consumers enforce their own freshness policy. Unavailable
fields are null. Non-retained delivery avoids leaving stale retained sensor
values after shutdown. A relay-only configuration uses MQTT without sending an
empty SUBSCRIBE packet or requiring a dummy subscription.
