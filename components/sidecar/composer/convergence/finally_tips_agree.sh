#!/usr/bin/env bash
# finally_tips_agree.sh — post-workload final-state check.
#
# Runs only after ALL drivers have completed naturally (no kill).
# Fault injection is stopped. This script asserts that once the
# workload is DONE (not mid-fault), every producer agrees on the tip.
# A divergent tip here means the test created a permanent fork.

set -u

# shellcheck disable=SC1091
source "$(dirname "$0")/helper_sdk_lib.sh"
# shellcheck disable=SC1091
source "$(dirname "$0")/helper_tip_probe_lib.sh"

POOLS="${POOLS:-3}"
PORT="${PORT:-3001}"
SLEEP_SETTLE="${SLEEP_SETTLE:-15}"
MAX_ATTEMPTS="${MAX_ATTEMPTS:-5}"
RETRY_DELAY="${RETRY_DELAY:-2}"

sdk_reachable "finally_tips_agree entered"

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
        echo "tips agree: $TIP_SUCCESSES"
        sdk_always true "all producer tips reachable at final check" \
            "$(jq -nc --argjson tips "$TIP_SUCCESSES" '{tips:$tips}')"
        sdk_always true "finally_tips_agree holds" \
            "$(jq -nc --argjson tips "$TIP_SUCCESSES" '{tips:$tips}')"
        exit 0
    fi
    sleep "$RETRY_DELAY"
done

echo "divergent/incomplete at end-of-workload: kind=$TIP_FAILURE_KIND reasons=$TIP_FAILURE_REASONS count=$TIP_COUNT distinct=$TIP_DISTINCT details=$TIP_DETAILS"
if [ "$TIP_COUNT" != "$POOLS" ]; then
    sdk_always false "all producer tips reachable at final check" \
        "$(jq -nc --arg kind "$TIP_FAILURE_KIND" \
                  --argjson tips "$TIP_DETAILS" \
                  --argjson count "$TIP_COUNT" \
                  --argjson distinct "$TIP_DISTINCT" \
                  --argjson reasons "$TIP_FAILURE_REASONS" \
            '{failure_kind:$kind, failure_reasons:$reasons, responses:$count, distinct_tips:$distinct, tips:$tips}')"
else
    sdk_always true "all producer tips reachable at final check" \
        "$(jq -nc --argjson tips "$TIP_SUCCESSES" '{tips:$tips}')"
    sdk_always false "finally_tips_agree holds" \
        "$(jq -nc --arg kind "$TIP_FAILURE_KIND" \
                  --argjson tips "$TIP_SUCCESSES" \
                  --argjson count "$TIP_COUNT" \
                  --argjson distinct "$TIP_DISTINCT" \
                  --argjson reasons "$TIP_FAILURE_REASONS" \
            '{failure_kind:$kind, failure_reasons:$reasons, responses:$count, distinct_tips:$distinct, tips:$tips}')"
fi
exit 1
