#!/usr/bin/env bash
# parallel_driver_heartbeat.sh — stub workload driver.
#
# Runs concurrently with fault injection. Each invocation does a short
# work cycle, emits one Sometimes assertion proving it reached the
# work step, and exits 0. Antithesis schedules many invocations across
# timelines; over a run this gives the report a populated drivers
# section without any real workload load.
#
# Exit 0 always — failure of this driver is not a SUT signal.

set -u

# shellcheck disable=SC1091
source "$(dirname "$0")/helper_sdk.sh"

sdk_reachable "stub heartbeat entered"

# Tiny work cycle: read the wall clock, sleep briefly, read it again.
T0=$(date +%s)
sleep 1
T1=$(date +%s)
DT=$((T1 - T0))

sdk_sometimes "$([ "$DT" -ge 1 ] && echo true || echo false)" \
    "stub heartbeat ticked" \
    "$(jq -nc --argjson dt "$DT" '{tick_seconds:$dt}')"

exit 0
