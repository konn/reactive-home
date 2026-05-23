---
name: haskell-format
description: |
  Use to format Haskell source files, including *.hs, *.lhs, and *.hsig, with the project's configured formatter: fourmolu, ormolu, or stylish-haskell. Use before compiling and after source edits.
---

# Haskell Source Formatting

Format Haskell source files before compiling. Cabal/project files are handled by the `haskell-cabal-gild` skill.

## Choosing A Formatter

Walk up from the file's directory and choose by project config:

1. `fourmolu.yaml` or `.fourmolu.yaml` selects `fourmolu`.
2. `.stylish-haskell.yaml` selects `stylish-haskell`.
3. Otherwise use the first available command in this order: `fourmolu`, `ormolu`, `stylish-haskell`.

This project has `fourmolu.yaml`, so prefer `fourmolu` when available.

## Format In Place

All supported formatters accept `-i`:

```sh
fourmolu -i <file>
ormolu -i <file>
stylish-haskell -i <file>
```

## Project Automation

This repository has a Codex `PostToolUse` hook in `.codex/hooks.json` that runs `.codex/hooks/haskell-format-post-tool-use.sh` after `apply_patch` edits. Re-read files if the hook reports that it changed them.

## Companion

Format `.cabal` and `cabal.project` files with the `haskell-cabal-gild` skill.
