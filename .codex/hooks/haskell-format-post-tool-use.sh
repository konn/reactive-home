#!/usr/bin/env bash
# Codex PostToolUse hook: format Haskell source files touched by apply_patch.
set -u

input="$(cat)"
command -v jq >/dev/null 2>&1 || exit 0

tool_name="$(printf '%s' "$input" | jq -r '.tool_name // empty')"
[ "$tool_name" = "apply_patch" ] || exit 0

patch="$(printf '%s' "$input" | jq -r '.tool_input.command // empty')"
[ -n "$patch" ] || exit 0

files="$(
  printf '%s\n' "$patch" |
    awk '
      /^\*\*\* Add File: / { sub(/^\*\*\* Add File: /, ""); print; next }
      /^\*\*\* Update File: / { sub(/^\*\*\* Update File: /, ""); print; next }
      /^\*\*\* Move to: / { sub(/^\*\*\* Move to: /, ""); print; next }
    ' |
    sort -u
)"
[ -n "$files" ] || exit 0

fmt=""
pick_formatter() {
  local file="$1"
  local dir
  dir="$(cd "$(dirname "$file")" 2>/dev/null && pwd)" || return 1

  while :; do
    if { [ -f "$dir/fourmolu.yaml" ] || [ -f "$dir/.fourmolu.yaml" ]; } && command -v fourmolu >/dev/null 2>&1; then
      fmt="fourmolu"; return 0
    fi
    if [ -f "$dir/.stylish-haskell.yaml" ] && command -v stylish-haskell >/dev/null 2>&1; then
      fmt="stylish-haskell"; return 0
    fi
    [ "$dir" = "/" ] && break
    dir="$(dirname "$dir")"
  done

  for f in fourmolu ormolu stylish-haskell; do
    command -v "$f" >/dev/null 2>&1 && { fmt="$f"; return 0; }
  done
  return 1
}

changed=""
while IFS= read -r file; do
  case "$(basename "$file")" in
    *.hs | *.lhs | *.hsig ) ;;
    * ) continue ;;
  esac
  [ -f "$file" ] || continue
  fmt=""
  pick_formatter "$file" || continue

  before="$(shasum "$file" 2>/dev/null | awk '{print $1}')"
  "$fmt" -i "$file" >/dev/null 2>&1 || true
  after="$(shasum "$file" 2>/dev/null | awk '{print $1}')"
  [ "$before" != "$after" ] && changed="${changed}${changed:+, }$file"
done <<EOF
$files
EOF

if [ -n "$changed" ]; then
  jq -cn --arg changed "$changed" '{
    hookSpecificOutput: {
      hookEventName: "PostToolUse",
      additionalContext: ("Formatted Haskell source on disk: " + $changed + ". Re-read changed files before further edits.")
    }
  }'
fi

exit 0
