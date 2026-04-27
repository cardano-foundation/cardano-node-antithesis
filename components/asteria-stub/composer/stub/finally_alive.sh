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
    cardano-cli ping -j --magic 42 \
        --host "p${1}.example" --port "$PORT" \
        --tip --quiet -c1 2>"$LAST_ERR" >/dev/null
}

LAST_ERR=$(mktemp)
LAST_RC=0
LAST_NODE=""
trap 'rm -f "$LAST_ERR"' EXIT

for attempt in $(seq 1 "$MAX_ATTEMPTS"); do
    ok=true
    for i in $(seq 1 "$POOLS"); do
        if ping_one "$i"; then
            :
        else
            LAST_RC=$?
            LAST_NODE="p$i"
            ok=false
            break
        fi
    done
    if $ok; then
        sdk_sometimes true "stub finally_alive holds" \
            "$(jq -nc --argjson a "$attempt" '{attempt:$a}')"
        exit 0
    fi
    sleep "$RETRY_DELAY"
done

ERR_TEXT="$(head -c 500 "$LAST_ERR" 2>/dev/null || true)"
sdk_sometimes false "stub finally_alive holds" \
    "$(jq -nc \
        --argjson a "$MAX_ATTEMPTS" \
        --arg node "$LAST_NODE" \
        --argjson rc "$LAST_RC" \
        --arg err "$ERR_TEXT" \
        '{attempts_exhausted:$a, last_node:$node, last_rc:$rc, last_err:$err}')"
exit 1
