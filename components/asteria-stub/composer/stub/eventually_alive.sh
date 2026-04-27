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
    # Match the sidecar's known-working invocation form. Returns
    # JSON on stdout when the handshake succeeds, exits non-zero
    # on failure. We capture stderr+exit so the assertion details
    # carry the actual cardano-cli error if any step fails.
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
        sdk_sometimes true "stub eventually_alive holds" \
            "$(jq -nc --argjson a "$attempt" '{attempt:$a}')"
        exit 0
    fi
    sleep "$RETRY_DELAY"
done

ERR_TEXT="$(head -c 500 "$LAST_ERR" 2>/dev/null || true)"
sdk_sometimes false "stub eventually_alive holds" \
    "$(jq -nc \
        --argjson a "$MAX_ATTEMPTS" \
        --arg node "$LAST_NODE" \
        --argjson rc "$LAST_RC" \
        --arg err "$ERR_TEXT" \
        '{attempts_exhausted:$a, last_node:$node, last_rc:$rc, last_err:$err}')"
exit 1
