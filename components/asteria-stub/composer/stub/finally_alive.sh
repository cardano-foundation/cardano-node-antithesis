#!/usr/bin/env bash
# finally_alive.sh — stub post-workload liveness probe.
#
# Runs after every parallel_driver completes (no fault injection). Same
# minimal shape as eventually_alive.sh — confirm the producers' N2N
# ports respond. End-of-run, not end-of-fault: a different signal from
# the eventually_ probe even though the implementation matches.

set -u

# shellcheck disable=SC1091
source "$(dirname "$0")/helper_sdk.sh"

POOLS="${POOLS:-3}"
PORT="${PORT:-3001}"
SLEEP_SETTLE="${SLEEP_SETTLE:-15}"
MAX_ATTEMPTS="${MAX_ATTEMPTS:-5}"
RETRY_DELAY="${RETRY_DELAY:-2}"

sdk_reachable "stub finally_alive entered"

sleep "$SLEEP_SETTLE"

ping_one() {
    cardano-cli ping --magic 42 \
        --host "p${1}.example" --port "$PORT" \
        --tip --quiet -c1 >/dev/null 2>&1
}

for attempt in $(seq 1 "$MAX_ATTEMPTS"); do
    ok=true
    for i in $(seq 1 "$POOLS"); do
        ping_one "$i" || { ok=false; break; }
    done
    if $ok; then
        sdk_always true "stub finally_alive holds" \
            "$(jq -nc --argjson a "$attempt" '{attempt:$a}')"
        exit 0
    fi
    sleep "$RETRY_DELAY"
done

sdk_always false "stub finally_alive holds" \
    "$(jq -nc --argjson a "$MAX_ATTEMPTS" '{attempts_exhausted:$a}')"
exit 1
