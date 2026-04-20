#!/usr/bin/env bash
# Launches a headless chromium (via nix) with remote-debugging and connects
# agent-browser to it via CDP. Works around the missing system libraries that
# prevent agent-browser's bundled Chrome from running on NixOS.
#
# Usage:
#   ./agent-browser-nixos.sh <session-name> <url> [agent-browser args...]
#
# Example:
#   ./agent-browser-nixos.sh triage "https://cardano.antithesis.com/report/..."
#   agent-browser --session triage get text body

set -euo pipefail

SESSION=${1:?session name required}
URL=${2:?url required}
shift 2

PORT=$((9222 + RANDOM % 1000))
USER_DATA=$(mktemp -d -t chrome-agent-XXXXXX)

cleanup() {
  [[ -n "${CPID:-}" ]] && kill "$CPID" 2>/dev/null || true
  rm -rf "$USER_DATA"
}
trap cleanup EXIT

chromium \
  --headless \
  --disable-gpu \
  --no-sandbox \
  --remote-debugging-port="$PORT" \
  --user-data-dir="$USER_DATA" \
  "about:blank" >/dev/null 2>&1 &
CPID=$!

# Wait for devtools to come up
for _ in {1..10}; do
  if curl -s "http://localhost:$PORT/json/version" >/dev/null 2>&1; then
    break
  fi
  sleep 0.3
done

agent-browser connect "$PORT" --session "$SESSION" >/dev/null
agent-browser open "$URL" --session "$SESSION" "$@"

# Keep chromium alive for subsequent agent-browser calls from another shell.
# Comment out the trap cleanup if you want to reuse the session.
echo "chromium pid=$CPID port=$PORT session=$SESSION"
echo "run further commands: agent-browser <cmd> --session $SESSION"
wait "$CPID"
