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
# shellcheck disable=SC1091
source "$(dirname "$0")/helper_tip_probe_lib.sh"

POOLS="${POOLS:-3}"
PORT="${PORT:-3001}"
# Intentionally short — this is one discrete probe, not a convergence wait.
SAMPLES="${SAMPLES:-3}"
SAMPLE_DELAY="${SAMPLE_DELAY:-1}"

sdk_reachable "serial_driver_tip_agreement entered"

for i in $(seq 1 "$SAMPLES"); do
    probe_all_tips
    if [ "$TIP_COUNT" != "$POOLS" ]; then
        sdk_sometimes true "tips unreachable during fault injection" \
            "$(jq -nc --argjson tips "$TIP_DETAILS" \
                      --argjson sample "$i" \
                      --arg kind "$TIP_FAILURE_KIND" \
                      --argjson reasons "$TIP_FAILURE_REASONS" \
                '{sample:$sample, failure_kind:$kind, failure_reasons:$reasons, tips:$tips}')"
    elif [ "$TIP_DISTINCT" = "1" ]; then
        sdk_sometimes true "tips agreed during fault injection" \
            "$(jq -nc --argjson tips "$TIP_SUCCESSES" --argjson sample "$i" \
                '{sample:$sample, tips:$tips}')"
    else
        sdk_sometimes true "tips diverged during fault injection" \
            "$(jq -nc --argjson tips "$TIP_SUCCESSES" \
                      --argjson sample "$i" \
                      --argjson distinct "$TIP_DISTINCT" \
                '{sample:$sample, distinct_tips:$distinct, tips:$tips}')"
    fi
    sleep "$SAMPLE_DELAY"
done

exit 0
