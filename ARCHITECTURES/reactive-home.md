# Reactive Home Architecture

`Home.Reactive.App` composes the MQTT input clock with a 500 ms Rhine heartbeat.
The MQTT stage parses ESPresense and Sesame events and tracks configured MQTT
switch states. Resampling buffers carry pending device events and the latest
switch snapshot into the heartbeat stage, which drives expiry, autolock, room
unlock rules, and scheduled switches. Mackerel reporting runs concurrently with
this network.

An optional SwitchBot sensor network runs alongside this MQTT network on its own
finite-scan Rhine clock. It starts even while the broker is connecting. Its
named readings and expiring snapshots feed independent local Hometrics REST and
optional MQTT relay workers. Each sensor may select its Hometrics device name
and measurement fields without changing its Rhine identity or readings; see
[SwitchBot sensors](switchbot.md) for the full
contract. MQTT relay is enabled per sensor with `mqtt_topic`. `host` is required
only when MQTT inputs or at least one sensor relay are enabled; `port` defaults
to 1883. A sensor-only configuration without relays does not initialize
an MQTT client. MQTT clients can also start with no subscriptions when used only
for relaying sensor data.

## Room Unlock and Dismissal

`Home.Reactive.Unlock` starts in `Waiting`. When the configured room stays empty
for `unlock.delay` with no approach detected, it can qualify as `Vacant` only if
every configured `unlock.dismiss` switch is off or absent. A switch such as
`do-not-disturb` therefore prevents new vacancy qualification while it is on.
When the last active dismissal switch turns off, both timers restart: auto-unlock
first rechecks room absence for `espresense.rooms.<unlock.room>.timeout`, then
requires a fresh `unlock.delay` of continuous vacancy with no approach or
dismissal. With a three-minute room timeout and a 30-second unlock delay, the
earliest new qualification is three minutes and 30 seconds after DND turns off.
Room presence returning during the delay restarts that delay. A later room
expiry can therefore postpone qualification further.

This recheck is local to auto-unlock. ESPresense snapshots and device `lastSeen`
timestamps remain actual observations; an empty snapshot during the recheck
cannot qualify a vacancy. `UnlockFeedback.rechecking` reports this interval,
`occupied` continues to report observed room presence, and `duration` counts
only eligible vacancy after the recheck. The app supplies the configured room
timeout to `unlockFeedbackS`/`unlockEventS`; an unknown unlock room is a startup
error. An off or absent switch at startup does not itself trigger a recheck.
Repeated off messages do not restart either timer, and with multiple dismissal
switches the reset occurs only when all are off or absent.

Once qualified, vacancy survives dismissal switches turning on. Clearing the
last active switch revokes that qualification before processing an approach on
the same heartbeat and starts the fresh timers above. Otherwise, room presence
without an approach moves `Vacant` to `ReadyForUnlock`; losing that presence
returns to `Vacant` without requiring another delay. An approach in either state
emits one `Unlock` regardless of dismissal switches and moves to `Occupied`.
Further approaches cannot unlock again until a new vacancy qualifies, which
still requires dismissal switches to be off. These rules apply only to room
unlock: Sesame `autolock_dismiss` continues to suppress both timer start and
firing.

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
