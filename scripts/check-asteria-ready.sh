#!/usr/bin/env bash
# Verify the actual indexer serves a ready snapshot and applies another block.
set -euo pipefail

container="${1:?usage: check-asteria-ready.sh CONTAINER [TIMEOUT_SECONDS]}"
budget="${2:-120}"
deadline=$((SECONDS + budget))
first_slot=""
last_reply=""

while (( SECONDS < deadline )); do
  # INDEXER_SOCK is expanded by bash inside the workload container.
  # shellcheck disable=SC2016
  last_reply="$(timeout 5 docker exec "$container" bash -c \
    'printf '\''{"ready":null}\n'\'' | timeout 2 socat - "UNIX-CONNECT:${INDEXER_SOCK:-/tmp/idx.sock}"' \
    2>/dev/null || true)"
  if jq -e '.ready == true and (.processedSlot | type == "number")
      and (.tipSlot | type == "number")
      and (.slotsBehind | type == "number") and .slotsBehind <= 5' \
      >/dev/null 2>&1 <<< "$last_reply"; then
    slot="$(jq -r '.processedSlot' <<< "$last_reply")"
    if [[ -z "$first_slot" ]]; then
      first_slot="$slot"
    elif (( slot > first_slot )); then
      printf 'PASS: Asteria indexer ready and advancing: %s -> %s\n' "$first_slot" "$slot"
      printf '%s\n' "$last_reply"
      exit 0
    fi
  fi
  sleep 1
done

printf 'FAIL: Asteria did not become ready and advance within %s seconds\n' "$budget" >&2
printf 'first_ready_slot=%s last_reply=%s\n' "${first_slot:-none}" "$last_reply" >&2
exit 1
