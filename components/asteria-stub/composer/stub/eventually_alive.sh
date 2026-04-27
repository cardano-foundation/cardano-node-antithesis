#!/usr/bin/env bash
# eventually_alive.sh — stub post-fault liveness probe.
#
# Antithesis stops fault injection before this script starts. The check
# is intentionally small: confirm cardano-cli is reachable on each
# producer's N2N port. The point is to fire the assertion, not to
# diagnose anything; harder convergence checks live in the sidecar
# template (`finally_tips_agree`, `eventually_converged`).
#
# Total budget: 15s settle + 5×2s retries = 25s worst case. Stays well
# under the 1h-test post-fault window so duration=1h passes the
# built-in "ran to completion" property without flakes.

set -u

# shellcheck disable=SC1091
source "$(dirname "$0")/helper_sdk.sh"

POOLS="${POOLS:-3}"
PORT="${PORT:-3001}"
SLEEP_SETTLE="${SLEEP_SETTLE:-15}"
MAX_ATTEMPTS="${MAX_ATTEMPTS:-5}"
RETRY_DELAY="${RETRY_DELAY:-2}"

sdk_reachable "stub eventually_alive entered"

sleep "$SLEEP_SETTLE"

ping_one() {
    cardano-cli ping --magic 42 \
        --host "p${1}.example" --port "$PORT" \
        --quiet -c1 >/dev/null 2>&1
}

for attempt in $(seq 1 "$MAX_ATTEMPTS"); do
    ok=true
    for i in $(seq 1 "$POOLS"); do
        ping_one "$i" || { ok=false; break; }
    done
    if $ok; then
        sdk_always true "stub eventually_alive holds" \
            "$(jq -nc --argjson a "$attempt" '{attempt:$a}')"
        exit 0
    fi
    sleep "$RETRY_DELAY"
done

sdk_always false "stub eventually_alive holds" \
    "$(jq -nc --argjson a "$MAX_ATTEMPTS" '{attempts_exhausted:$a}')"
exit 1
