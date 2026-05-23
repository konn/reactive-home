---
name: haskell
description: |
  Use for Haskell development in this project: editing .hs/.lhs/.hsig/.cabal/package.yaml/cabal.project files, typechecking or building with cabal, fixing compiler errors, changing dependencies, or looking up Haskell APIs. Prefer cabal nix-style builds, package-by-package validation, HLS diagnostics when available, local docs, Haddock, and Hoogle.
---

# Haskell Development

This project uses cabal nix-style builds. Treat `cabal.project` and package `.cabal` files as the source of truth.

## Ground Rules

- Use `cabal`, not `stack`, and do not invoke `ghc` directly for normal validation.
- In this multi-package project, build one package at a time with `cabal build <pkg>` before moving to another package.
- Format changed Haskell source with the `haskell-format` skill before building.
- Format changed `.cabal` and `cabal.project` files with the `haskell-cabal-gild` skill before building.
- Prefer `(<>)` over `(++)` for concatenation, including strings and lists.
- After editing `package.yaml`, run `hpack` to regenerate the corresponding `.cabal` file.

## Workflow

1. Inspect the relevant package and existing conventions before editing.
2. Edit the smallest reasonable surface.
3. Use HLS diagnostics, hover/types, definitions, references, or rename when available before running a full build.
4. Look up APIs as needed:
   - Prefer local Haddock or installed package docs when available.
   - Use the project-local `haddock` skill for dependency docs or source.
   - Use the project-local `hoogle` skill or local `hoogle` command for name/type search.
   - Use remote Hoogle or Hackage only when local search is insufficient.
5. Build with `cabal build <pkg>` once quick diagnostics are clean or unavailable.
6. Test with `cabal test <pkg>` or the specific test target relevant to the change.

## Dependency Changes

- Edit `.cabal`, `package.yaml`, or `cabal.project` according to the project’s current layout.
- Run `hpack` after `package.yaml` edits.
- Reformat cabal/project files.
- Run `cabal build <pkg>` for affected packages; run `cabal build all` only when a dependency or build-plan change needs broad validation or refreshed local docs.

## Migrated Claude Plugins

- `haskell-skill@konn-haskell-claude-tools` maps to this skill.
- `hoogle@claude-hoogle` maps to `.codex/skills/hoogle`.
- Haddock/Hackage documentation lookup maps to `.codex/skills/haddock`.
- Claude hook-based formatting maps to the `haskell-format` and `haskell-cabal-gild` skills plus real Codex hooks in `.codex/hooks.json` and `.codex/hooks/`.
- The Claude HLS plugin maps to this workflow instruction: prefer HLS when available. Codex does not consume `.lsp.json` as a skill.
