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

POOLS="${POOLS:-3}"
PORT="${PORT:-3001}"
SLEEP_SETTLE="${SLEEP_SETTLE:-15}"
MAX_ATTEMPTS="${MAX_ATTEMPTS:-10}"
RETRY_DELAY="${RETRY_DELAY:-2}"

sdk_reachable "eventually_converged entered"

# Faults are off. Allow the system to settle. 15s is the canonical
# Antithesis recovery allowance quoted in their etcd example.
sleep "$SLEEP_SETTLE"

NODES=()
for i in $(seq 1 "$POOLS"); do
    NODES+=("p${i}")
done

query_tip_json() {
    # $1 = host prefix (e.g. p1)
    cardano-cli ping -j \
        --magic 42 --host "${1}.example" --port "$PORT" \
        --tip --quiet -c1 2>/dev/null
}

# Populates TIP_DETAILS (jq array), TIP_DISTINCT (count of unique
# non-null hashes), TIP_COUNT (number of successful responses).
collect_tips() {
    local temp_dir node
    temp_dir="$(mktemp -d)"
    for node in "${NODES[@]}"; do
        (query_tip_json "$node" > "$temp_dir/$node") &
    done
    wait
    # Combine into an array of {node, hash, block, slot} entries, dropping
    # nodes that failed to respond (they have empty / invalid JSON files).
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
        echo "converged on attempt $attempt: $TIP_DETAILS"
        sdk_sometimes true "eventually_converged succeeded" \
            "$(jq -nc --argjson tips "$TIP_DETAILS" --argjson attempt "$attempt" \
                '{attempt:$attempt, tips:$tips}')"
        exit 0
    fi
    sleep "$RETRY_DELAY"
done

# Did not converge within the retry budget. Real recovery failure.
echo "did not converge after $MAX_ATTEMPTS attempts: count=$TIP_COUNT distinct=$TIP_DISTINCT"
sdk_unreachable "eventually_converged failed to converge" \
    "$(jq -nc --argjson attempts "$MAX_ATTEMPTS" \
              --argjson count "$TIP_COUNT" \
              --argjson distinct "$TIP_DISTINCT" \
              --argjson tips "$TIP_DETAILS" \
        '{attempts:$attempts, responses:$count, distinct_tips:$distinct, tips:$tips}')"
exit 1
