#!/bin/sh
# Tail cardano-tracer ForMachine JSON logs, emit only Warning+ events to stdout.
# stdout is captured by Antithesis Logs Explorer (indexed as source=log-tailer).
#
# cardano-tracer rotates files periodically — we poll for new files every 15s
# and spawn one background `tail -F` per file, so rotation is picked up without
# missing events.

set -eu

LOG_ROOT="${LOG_ROOT:-/tracer/logs}"
POLL_INTERVAL="${POLL_INTERVAL:-15}"
PATTERN="${PATTERN:-\"sev\":\"(Warning|Error|Critical)\"}"

SEEN=/tmp/log-tailer-seen
mkdir -p "$SEEN"

start_tail() {
  f="$1"
  key=$(printf '%s' "$f" | tr -c 'a-zA-Z0-9' _)
  [ -e "$SEEN/$key" ] && return
  touch "$SEEN/$key"
  tail -n +1 -F "$f" 2>/dev/null \
    | awk -v p="$PATTERN" '$0 ~ p { print; fflush() }' &
}

while :; do
  for f in "$LOG_ROOT"/*/node-*.json; do
    [ -f "$f" ] && start_tail "$f"
  done
  sleep "$POLL_INTERVAL"
done
