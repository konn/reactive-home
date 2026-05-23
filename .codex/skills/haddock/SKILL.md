---
name: haddock
description: |
  Use to read Haskell package documentation, Haddock HTML, Hackage pages, or dependency source for cabal nix-style projects. Prefer locally built/cached docs and source from the cabal store, dist-newstyle, plan.json, and repo-cache; fall back to Hackage only for packages outside the project dependency set or when local docs are unavailable.
---

# Haddock And Hackage Lookup

Read dependency documentation and source for cabal nix-style projects, preferring local files over network access.

## Source Selection

- If the package is in the current cabal project dependency plan, read it locally.
- If the package is outside the dependency set or local docs are absent, fall back to Hackage.
- Prefer the `hoogle` skill for symbol/type search before fetching full documentation pages.

## Local Setup

If docs are missing, enable documentation and build once:

```sh
cabal configure --enable-documentation
cabal build all
```

Use `cabal path --output-format=json` to find:

- `compiler.store-path`, which contains built packages and HTML docs.
- `remote-repo-cache`, which contains downloaded Hackage source tarballs.

## Resolving Dependencies

1. Read `dist-newstyle/cache/plan.json`.
2. Find the entry in `install-plan[]` for the package.
3. Use its `id` as the exact unit id. Do not construct store paths from the package name, because cabal abbreviates and hashes store entries.
4. Note `pkg-name`, `pkg-version`, and `pkg-src.type`.

## Hackage Or Stackage Packages

For `pkg-src.type = repo-tar`:

- HTML docs are under `<compiler-store-path>/<UnitId>/share/doc/**/html/index.html`.
- Module pages usually replace `.` with `-`, such as `System.Random.SplitMix` to `System-Random-SplitMix.html`.
- Source tarballs are under `<remote-repo-cache>/hackage.haskell.org/<pkg-name>/<pkg-version>/<pkg-name>-<pkg-version>.tar.gz`.

List or stream tarball contents without unpacking the whole archive:

```sh
tar -tzf <tarball>
tar -xzOf <tarball> <pkg>-<ver>/<path/to/File.hs>
```

## Source Repo Or Local Packages

For `pkg-src.type = source-repo` or `local`, inspect files under `dist-newstyle/src/` or the local package path from `cabal.project`.

## Hackage Fallback

Use Hackage only when local lookup is not available:

```text
https://hackage.haskell.org/package/<pkg>
https://hackage.haskell.org/package/<pkg>-<ver>/docs/<Module-With-Dashes>.html
```
