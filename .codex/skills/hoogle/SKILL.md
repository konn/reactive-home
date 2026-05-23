---
name: hoogle
description: |
  Use to search Hoogle for Haskell functions, type signatures, packages, modules, and documentation. Trigger when a task needs Haskell API discovery, signature lookup, or a function by type/name.
---

# Hoogle Search

Use Hoogle to find Haskell APIs by name, type signature, package, or module.

## Preferred Procedure

1. Start with a local Hoogle command if available:

   ```sh
   hoogle '<query>'
   ```

   If `hoogle` is not on `PATH`, try `/Users/hiromi/.cabal/bin/hoogle`.

2. For remote lookup, query Hoogle’s JSON endpoint:

   ```text
   https://hoogle.haskell.org/?hoogle=<URL_ENCODED_QUERY>&mode=json
   ```

3. Return the most relevant results, usually the top 5 to 10.

## Result Format

For each useful result, include:

- Function or item name and cleaned signature.
- Package and module.
- A short documentation summary.
- Documentation URL when available.

Strip HTML tags and decode common entities such as `&gt;`, `&lt;`, and `&amp;` before presenting results.

## Failure Cases

- If there are no results, say so and suggest a broader name or a type-signature query.
- If remote lookup fails, report the failure and suggest visiting `https://hoogle.haskell.org` directly.
