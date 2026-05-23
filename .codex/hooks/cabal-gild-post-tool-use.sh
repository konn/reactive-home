#!/usr/bin/env bash
# Codex PostToolUse hook: run cabal-gild for cabal/project files touched by apply_patch,
# and refresh nearby cabal-gild discovered module lists after Haskell source edits.
set -u

input="$(cat)"
command -v jq >/dev/null 2>&1 || exit 0
command -v cabal-gild >/dev/null 2>&1 || exit 0

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

gild() { cabal-gild --io "$1" >/dev/null 2>&1 || true; }

targets=""
while IFS= read -r file; do
  [ -n "$file" ] || continue
  case "$(basename "$file")" in
    *.cabal | cabal.project | cabal.project.* )
      [ -f "$file" ] && targets="${targets}${targets:+
}$file"
      ;;
    *.hs | *.lhs | *.hsig )
      [ -f "$file" ] || continue
      dir="$(cd "$(dirname "$file")" 2>/dev/null && pwd)" || continue
      while :; do
        if ls "$dir"/*.cabal >/dev/null 2>&1; then
          for cabal_file in "$dir"/*.cabal; do
            grep -Eq 'cabal-gild:[[:space:]]*discover' "$cabal_file" 2>/dev/null || continue
            targets="${targets}${targets:+
}$cabal_file"
          done
          break
        fi
        [ "$dir" = "/" ] && break
        dir="$(dirname "$dir")"
      done
      ;;
  esac
done <<EOF
$files
EOF

targets="$(printf '%s\n' "$targets" | sed '/^$/d' | sort -u)"
[ -n "$targets" ] || exit 0

changed=""
while IFS= read -r file; do
  [ -f "$file" ] || continue
  before="$(shasum "$file" 2>/dev/null | awk '{print $1}')"
  gild "$file"
  after="$(shasum "$file" 2>/dev/null | awk '{print $1}')"
  [ "$before" != "$after" ] && changed="${changed}${changed:+, }$file"
done <<EOF
$targets
EOF

if [ -n "$changed" ]; then
  jq -cn --arg changed "$changed" '{
    hookSpecificOutput: {
      hookEventName: "PostToolUse",
      additionalContext: ("cabal-gild formatted files on disk: " + $changed + ". Re-read changed files before further edits.")
    }
  }'
fi

exit 0
