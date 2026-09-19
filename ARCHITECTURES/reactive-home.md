# Reactive Home Architecture

`Home.Reactive.App` composes the MQTT input clock with a 500 ms Rhine heartbeat.
The MQTT stage parses ESPresense and Sesame events and tracks configured MQTT
switch states. Resampling buffers carry pending device events and the latest
switch snapshot into the heartbeat stage, which drives expiry, autolock, room
unlock rules, and scheduled switches. Mackerel reporting runs concurrently with
this network.

## Scheduled MQTT Switches

`Home.Reactive.MQTT.MqttDevices` accepts optional `switches` and
`scheduled_switches` table arrays. A scheduled switch has `name`, `topic`,
`interval`, and `on_duration` fields. `Home.Reactive.Duration` supplies the shared
duration codec; ESPresense re-exports its previous duration API. Scheduled switch
decoding requires finite positive durations with `on_duration < interval`.

`Home.Reactive.ScheduledSwitch` separates heartbeat-driven state transitions
from MQTT publishing. Each configured switch has its own timer. The first tick
announces OFF; the first ON is due one interval after clock initialization.
Subsequent activations stay on that interval cadence, and the OFF deadline is
measured from publication completion, so waiting for the broker cannot shorten
the ON duration. A publishing callback supplies the completion timestamp; pure
timer tests use the tick timestamp instead. Late ticks skip missed
activations rather than replaying them. If OFF is processed after another
activation was due, the next ON waits for the next future cadence boundary.
Schedules reset with the process and use the existing heartbeat's time base and
500 ms resolution. Durations or OFF gaps shorter than the heartbeat resolution
can be stretched or cause an activation to be skipped.

Transitions publish UTF-8 `true` or `false` to the configured topic, at QoS 1
with retention enabled, using the existing reconnecting MQTT effect. The
heartbeat runs independently of incoming MQTT messages. Scheduled topics are
included in subscriptions, allowing an application with only scheduled switches
to start. These subscriptions set `noLocal = False` so the application receives
its own announcements. The MQTT snapshot treats scheduled topics as ordinary
boolean switches, allowing their names in dismissal conditions; other
subscriptions retain their existing no-local behavior.

Publication runs in the heartbeat stage like the existing autolock commands.
Broker unavailability can delay publication and heartbeat processing; the timers
do not provide a delivery deadline while disconnected. A broker rejection raises
`ScheduledSwitchPublishError` rather than being treated as a successful state
announcement. Retained states describe
the last published value and are reset to OFF when the scheduler starts again.
