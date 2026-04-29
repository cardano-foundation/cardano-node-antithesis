#!/usr/bin/env bash
# eventually_population_grew.sh — short single-shot
# validator (≤1 min total per the antithesis-tests skill).
# Snapshots the daemon's population size and asserts it
# is non-zero. Fires after the post-fault settle window.

set -euo pipefail
SHELL="/bin/bash"
PATH="/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin:$PATH"

source "$(dirname "$0")/helper_sdk_lib.sh"

CONTROL_SOCKET="${CONTROL_SOCKET:-/state/tx-generator-control.sock}"

sdk_reachable "tx_generator_eventually_started"

# Let the post-fault window settle; the daemon may have
# been killed mid-transact and need a moment to reattach
# its N2C connection.
sleep 15

RSP="$(printf '{"snapshot":null}\n' | nc -U -q 1 "$CONTROL_SOCKET" || true)"
POP="$(printf '%s' "$RSP" | jq -r '.populationSize // 0')"

if [ "$POP" -gt 0 ]; then
    sdk_sometimes true "tx_generator_population_grew" \
        "$(printf '{"populationSize":%s}' "$POP")"
    exit 0
fi

sdk_unreachable "tx_generator_population_did_not_grow" \
    "$(printf '{"populationSize":%s}' "$POP")"
exit 1
