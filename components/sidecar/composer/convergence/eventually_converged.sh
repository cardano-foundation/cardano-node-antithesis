#!/usr/bin/env bash
# eventually_converged.sh — post-fault convergence check.
#
# Antithesis stops fault injection before this script starts and kills
# every other non-anytime command on this timeline. This script runs
# alone, validates recovery in bounded time, and exits.
#
# Total wall time: 15s settle + up to 10×2s retry budget = 35s worst case.
#
# Contract (built-in properties enforce this):
#   - Must complete in significantly less than the test's post-fault window.
#   - Must emit a Reachable assertion on entry so the report can show
#     "this command fired on at least one timeline".
#   - Exit 0 on convergence, 1 on failure to converge.

set -u

# shellcheck disable=SC1091
source "$(dirname "$0")/helper_sdk_lib.sh"
# shellcheck disable=SC1091
source "$(dirname "$0")/helper_tip_probe_lib.sh"

POOLS="${POOLS:-3}"
PORT="${PORT:-3001}"
SLEEP_SETTLE="${SLEEP_SETTLE:-15}"
MAX_ATTEMPTS="${MAX_ATTEMPTS:-10}"
RETRY_DELAY="${RETRY_DELAY:-2}"

sdk_reachable "eventually_converged entered"

# Faults are off. Allow the system to settle. 15s is the canonical
# Antithesis recovery allowance quoted in their etcd example.
sleep "$SLEEP_SETTLE"

TIP_DETAILS="[]"
TIP_SUCCESSES="[]"
TIP_FAILURES="[]"
TIP_COUNT=0
TIP_DISTINCT=0
TIP_FAILURE_REASONS="[]"
TIP_FAILURE_KIND="not_checked"
for attempt in $(seq 1 "$MAX_ATTEMPTS"); do
    probe_all_tips
    if [ "$TIP_COUNT" = "$POOLS" ] && [ "$TIP_DISTINCT" = "1" ]; then
        echo "converged on attempt $attempt: $TIP_SUCCESSES"
        sdk_sometimes true "eventually_converged succeeded" \
            "$(jq -nc --argjson tips "$TIP_SUCCESSES" --argjson attempt "$attempt" \
                '{attempt:$attempt, tips:$tips}')"
        exit 0
    fi
    sleep "$RETRY_DELAY"
done

# Did not converge within the retry budget. Real recovery failure.
echo "did not converge after $MAX_ATTEMPTS attempts: kind=$TIP_FAILURE_KIND reasons=$TIP_FAILURE_REASONS count=$TIP_COUNT distinct=$TIP_DISTINCT details=$TIP_DETAILS"
sdk_unreachable "eventually_converged failed: $TIP_FAILURE_KIND" \
    "$(jq -nc --arg kind "$TIP_FAILURE_KIND" \
              --argjson attempts "$MAX_ATTEMPTS" \
              --argjson count "$TIP_COUNT" \
              --argjson distinct "$TIP_DISTINCT" \
              --argjson reasons "$TIP_FAILURE_REASONS" \
              --argjson tips "$TIP_DETAILS" \
        '{failure_kind:$kind, failure_reasons:$reasons, attempts:$attempts, responses:$count, distinct_tips:$distinct, tips:$tips}')"
exit 1
