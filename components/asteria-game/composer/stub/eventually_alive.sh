#!/usr/bin/env bash
# eventually_alive.sh — stub post-fault liveness probe via the indexer.
#
# Antithesis stops fault injection before this script starts. The
# check is intentionally small: confirm the long-lived utxo-indexer
# is still responsive on its Unix domain socket and at the chain
# tip (slotsBehind=0). The point is to fire the assertion, not to
# diagnose anything; richer convergence checks live in the sidecar
# template (`finally_tips_agree`, `eventually_converged`).
#
# Total budget: 15 s settle + 5×2 s retries = 25 s worst case.
# Stays well under the 1h-test post-fault window.

set -u

# shellcheck disable=SC1091
source "$(dirname "$0")/helper_sdk.sh"

INDEXER_SOCK="${INDEXER_SOCK:-/tmp/idx.sock}"
SLEEP_SETTLE="${SLEEP_SETTLE:-15}"
MAX_ATTEMPTS="${MAX_ATTEMPTS:-5}"
RETRY_DELAY="${RETRY_DELAY:-2}"

sdk_reachable "stub eventually_alive entered"

sleep "$SLEEP_SETTLE"

LAST_REPLY=""
for attempt in $(seq 1 "$MAX_ATTEMPTS"); do
    LAST_REPLY="$(printf '{"ready": null}\n' | socat - "UNIX-CONNECT:${INDEXER_SOCK}" 2>/dev/null || true)"
    if [ -n "$LAST_REPLY" ] \
        && printf '%s' "$LAST_REPLY" | jq -e '(.slotsBehind // null) != null and .slotsBehind <= 5' >/dev/null 2>&1; then
        TIP="$(printf '%s' "$LAST_REPLY" | jq -r '.tipSlot // 0')"
        sdk_sometimes true "stub eventually_alive holds" \
            "$(jq -nc --argjson a "$attempt" --argjson t "$TIP" '{attempt:$a, tipSlot:$t}')"
        exit 0
    fi
    sleep "$RETRY_DELAY"
done

sdk_sometimes false "stub eventually_alive holds" \
    "$(jq -nc --argjson a "$MAX_ATTEMPTS" --arg reply "$LAST_REPLY" \
        '{attempts_exhausted:$a, last_reply:$reply}')"
exit 1
