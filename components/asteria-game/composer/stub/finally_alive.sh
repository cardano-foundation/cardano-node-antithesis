#!/usr/bin/env bash
# finally_alive.sh — stub post-workload liveness probe via the indexer.
#
# Runs after every parallel_driver completes (no fault injection).
# Same shape as eventually_alive.sh — confirm the indexer is at the
# chain tip. End-of-run, not end-of-fault: a different signal from
# the eventually_ probe even though the implementation matches.
#
# Budget: 3 s settle + 8×1 s retries = 11 s worst case. Same tight
# bound as eventually_alive — finally_ commands have a longer composer
# cap (~54 s observed) but there's no benefit to a longer settle window
# at end-of-run, and the tight budget makes the script robust to
# whatever the actual cap turns out to be.
#
# Always exits 0 — the SDK records the outcome; a non-zero shell exit
# would duplicate the signal under the composer's "Always: zero exit
# code" property.

set -u

# shellcheck disable=SC1091
source "$(dirname "$0")/helper_sdk.sh"

INDEXER_SOCK="${INDEXER_SOCK:-/tmp/idx.sock}"
SLEEP_SETTLE="${SLEEP_SETTLE:-3}"
MAX_ATTEMPTS="${MAX_ATTEMPTS:-8}"
RETRY_DELAY="${RETRY_DELAY:-1}"

sdk_reachable "stub finally_alive entered"

sleep "$SLEEP_SETTLE"

LAST_REPLY=""
for attempt in $(seq 1 "$MAX_ATTEMPTS"); do
    LAST_REPLY="$(printf '{"ready": null}\n' | socat - "UNIX-CONNECT:${INDEXER_SOCK}" 2>/dev/null || true)"
    if [ -n "$LAST_REPLY" ] \
        && printf '%s' "$LAST_REPLY" | jq -e '(.slotsBehind // null) != null and .slotsBehind <= 5' >/dev/null 2>&1; then
        TIP="$(printf '%s' "$LAST_REPLY" | jq -r '.tipSlot // 0')"
        sdk_sometimes true "stub finally_alive holds" \
            "$(jq -nc --argjson a "$attempt" --argjson t "$TIP" '{attempt:$a, tipSlot:$t}')"
        exit 0
    fi
    sleep "$RETRY_DELAY"
done

sdk_sometimes false "stub finally_alive holds" \
    "$(jq -nc --argjson a "$MAX_ATTEMPTS" --arg reply "$LAST_REPLY" \
        '{attempts_exhausted:$a, last_reply:$reply}')"
exit 0
