# haskesame-mqtt

MQTT bridge helpers for BLE-based Sesame 5 clients.

This package follows the `ssm2mqtt` topic and payload mapping:

* subscribe to `<base-topic>/+/set`
* accept command payloads `LOCKED` and `UNLOCKED`
* publish retained QoS 1 status JSON to `<base-topic>/<sesame-uuid>/get`

Status payloads contain:

```json
{
  "position": -13,
  "lockCurrentState": "LOCKED",
  "batteryVoltage": 6.062,
  "batteryLevel": 100,
  "chargingState": "NOT_CHARGEABLE",
  "statusLowBattery": false
}
```

## Copyright

2026-present (c) Hiromi ISHII
