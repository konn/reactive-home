# Changelog for `reactive-home`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

- Add optional `unlock.confirm` sensor/device distance and freshness conditions,
  configurable in TOML with an empty list restoring approach-only auto-unlock.
  Preserve qualified vacancy while waiting for confirmation and report the
  confirmation result in unlock feedback.

## 0.1.0.0 - YYYY-MM-DD
