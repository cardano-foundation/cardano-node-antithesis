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

POOLS="${POOLS:-3}"
PORT="${PORT:-3001}"
SLEEP_SETTLE="${SLEEP_SETTLE:-15}"
MAX_ATTEMPTS="${MAX_ATTEMPTS:-5}"
RETRY_DELAY="${RETRY_DELAY:-2}"

sdk_reachable "finally_tips_agree entered"

sleep "$SLEEP_SETTLE"

NODES=()
for i in $(seq 1 "$POOLS"); do
    NODES+=("p${i}")
done

query_tip_json() {
    cardano-cli ping -j \
        --magic 42 --host "${1}.example" --port "$PORT" \
        --tip --quiet -c1 2>/dev/null
}

collect_tips() {
    local temp_dir node
    temp_dir="$(mktemp -d)"
    for node in "${NODES[@]}"; do
        (query_tip_json "$node" > "$temp_dir/$node") &
    done
    wait
    TIP_DETAILS="$(for node in "${NODES[@]}"; do
        jq -c --arg node "$node" \
            'if .tip[0] then
                {node:$node, hash:.tip[0].hash, block:.tip[0].blockNo, slot:.tip[0].slotNo}
             else empty end' \
            "$temp_dir/$node" 2>/dev/null
    done | jq -sc '.')"
    rm -rf "$temp_dir"
    TIP_COUNT="$(printf '%s' "$TIP_DETAILS" | jq 'length')"
    TIP_DISTINCT="$(printf '%s' "$TIP_DETAILS" | jq '[.[].hash] | unique | length')"
}

TIP_DETAILS="[]"
TIP_COUNT=0
TIP_DISTINCT=0
for attempt in $(seq 1 "$MAX_ATTEMPTS"); do
    collect_tips
    if [ "$TIP_COUNT" = "$POOLS" ] && [ "$TIP_DISTINCT" = "1" ]; then
        echo "tips agree: $TIP_DETAILS"
        sdk_always true "finally_tips_agree holds" \
            "$(jq -nc --argjson tips "$TIP_DETAILS" '{tips:$tips}')"
        exit 0
    fi
    sleep "$RETRY_DELAY"
done

echo "divergent/incomplete at end-of-workload: count=$TIP_COUNT distinct=$TIP_DISTINCT"
sdk_always false "finally_tips_agree holds" \
    "$(jq -nc --argjson tips "$TIP_DETAILS" \
              --argjson count "$TIP_COUNT" \
              --argjson distinct "$TIP_DISTINCT" \
        '{responses:$count, distinct_tips:$distinct, tips:$tips}')"
exit 1
