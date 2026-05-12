#!/usr/bin/env bash
# eventually_population_grew.sh — short single-shot
# validator (≤1 min total per the antithesis-tests skill).
# Snapshots the daemon's population size and asserts it
# is non-zero. Fires after the post-fault settle window.
#
# Always exits 0. See parallel_driver_transact.sh for
# the rationale.

set -u
SHELL="/bin/bash"
PATH="/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin:$PATH"

# See parallel_driver_transact.sh for why we don't
# 'set -e' here.

source "$(dirname "$0")/helper_sdk_lib.sh"
sdk_install_signal_trap "tx_generator_eventually_signal"

CONTROL_SOCKET="${CONTROL_SOCKET:-/state/tx-generator-control.sock}"

sdk_reachable "tx_generator_eventually_started"

# Let the post-fault window settle; the daemon may have
# been killed mid-transact and need a moment to reattach
# its N2C connection.
sleep 15

# Hard 5s wall-clock on the snapshot — see parallel_driver_*.sh
# for the rationale (daemon accept-loop wedging under a fault
# window can otherwise hang past the composer's per-step
# deadline).
RSP="$(control_socket_request "$CONTROL_SOCKET" '{"snapshot":null}')"

# If the daemon isn't reachable (control socket not yet
# bound, supervisor mid-cycle, 5s timeout above firing,
# etc.) we can't make a claim about the population either
# way. Surface a reachability event and exit 0; do NOT fire
# the 'did_not_grow' assertion against a daemon we never
# successfully queried.
if [ -z "$RSP" ]; then
    sdk_reachable "tx_generator_eventually_daemon_unreachable"
    exit 0
fi

# Reject responses that aren't valid JSON (defensive —
# the daemon may have written a partial line).
if ! POP="$(printf '%s' "$RSP" | jq -e -r '.populationSize // 0' 2>/dev/null)"; then
    sdk_reachable "tx_generator_eventually_daemon_unreachable"
    exit 0
fi

# Distinguish "no refill has been attempted yet" (lastTxId
# is null in the snapshot, populationSize=0 trivially)
# from "refill ran but population genuinely failed to
# grow". The Always assertion 'did_not_grow' should only
# fire on the latter — otherwise it spuriously trips on
# every fork where the validator runs before the first
# refill could land (e.g. vtime 42-48s on the run at
# https://cardano.antithesis.com/report/JjPofIAbSixtn9DexNDJLl9I).
LAST_TX_ID="$(printf '%s' "$RSP" | jq -r '.lastTxId // ""' 2>/dev/null || echo "")"

if [ "$POP" -gt 0 ]; then
    sdk_sometimes true "tx_generator_population_grew" \
        "$(printf '{"populationSize":%s}' "$POP")"
elif [ -z "$LAST_TX_ID" ]; then
    sdk_reachable "tx_generator_eventually_no_tx_yet" \
        "$(printf '{"populationSize":%s}' "$POP")"
else
    sdk_unreachable "tx_generator_population_did_not_grow" \
        "$(jq -nc --argjson p "$POP" --arg t "$LAST_TX_ID" \
            '{populationSize:$p, lastTxId:$t}')"
fi
exit 0
