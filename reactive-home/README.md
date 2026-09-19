# reactive-home

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
