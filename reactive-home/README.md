# reactive-home

## Configuration

`clientId` is optional. When omitted, `reactive-home` asks the MQTT broker to
assign a unique client identifier, which avoids client-id takeover conflicts if a
previous interrupted process is still disconnecting or reconnecting. Set
`clientId = "..."` only when a stable MQTT client identity is required.

## Copyright

(c) Hiromi ISHII 2026- present
