# Changelog for `hasquitto-auto-reconnect`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.1.0.0 - YYYY-MM-DD

- Initial release: an auto-reconnecting, auto-resubscribing MQTT v5 client
  (`Network.Mqtt.Client.AutoReconnect`) layered on the public API of
  `hasquitto-core`.
  - Background supervisor with full-jitter exponential backoff (`BackoffConfig`)
    and a `maxRetries` cap.
  - Block-until-reconnected for operations issued while disconnected; in-flight
    operations surface their connection-lost error.
  - Subscription registry replayed after a fresh-session reconnect, with
    partial-failure reporting via `onResubscribe`.
  - `onReconnect` hook, `waitClosed`, `status`, and `isConnected` for lifecycle
    observation.
