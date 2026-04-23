#!/usr/bin/env bash
# serial_driver_tip_agreement.sh — exclusive-access tip coherence probe.
#
# Runs with exclusive access: while this script is running, no
# parallel_driver runs on this timeline (only anytime_ commands).
# Faults ARE active during this run. The goal is to sample the tip on
# every producer under fault conditions and register how often they
# diverge (Sometimes) vs hold (Always on the final attempt).
#
# Keeps the serial_driver category populated with a real useful
# operation — replaces the no-op sleep pacer that used to live here.

set -u

# shellcheck disable=SC1091
source "$(dirname "$0")/helper_sdk_lib.sh"

POOLS="${POOLS:-3}"
PORT="${PORT:-3001}"
# Intentionally short — this is one discrete probe, not a convergence wait.
SAMPLES="${SAMPLES:-3}"
SAMPLE_DELAY="${SAMPLE_DELAY:-1}"

sdk_reachable "serial_driver_tip_agreement entered"

NODES=()
for i in $(seq 1 "$POOLS"); do
    NODES+=("p${i}")
done

sample_once() {
    local out
    out="$(for node in "${NODES[@]}"; do
        cardano-cli ping -j \
            --magic 42 --host "${node}.example" --port "$PORT" \
            --tip --quiet -c1 2>/dev/null \
            | jq -c --arg node "$node" '{node:$node, hash:.tip[0].hash, block:.tip[0].blockNo}'
    done | jq -sc '.')"
    printf '%s' "$out"
}

for i in $(seq 1 "$SAMPLES"); do
    details="$(sample_once)"
    distinct="$(printf '%s' "$details" | jq '[.[] | .hash] | unique | length')"
    if [ "$distinct" = "1" ]; then
        sdk_sometimes true "tips agreed during fault injection" \
            "$(jq -nc --argjson tips "$details" --argjson sample "$i" \
                '{sample:$sample, tips:$tips}')"
    else
        sdk_sometimes true "tips diverged during fault injection" \
            "$(jq -nc --argjson tips "$details" \
                      --argjson sample "$i" \
                      --argjson distinct "$distinct" \
                '{sample:$sample, distinct_tips:$distinct, tips:$tips}')"
    fi
    sleep "$SAMPLE_DELAY"
done

exit 0
