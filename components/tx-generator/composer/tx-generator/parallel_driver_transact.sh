#!/usr/bin/env bash
# parallel_driver_transact.sh — fire one transact request
# at the cardano-tx-generator daemon. Reads an Antithesis
# random uint64 (or falls back to /dev/urandom outside the
# Antithesis runtime) and sends a single NDJSON request
# over the daemon's control socket. Exits 0 on a landed
# transaction, exits 1 on a not-applicable response so
# Antithesis records the tick as "command not applicable
# this iteration" rather than a failure.
#
# Wire schema lives in
# https://github.com/lambdasistemi/cardano-node-clients/blob/main/specs/034-cardano-tx-generator/contracts/control-wire.md

set -euo pipefail
SHELL="/bin/bash"
PATH="/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin:$PATH"

source "$(dirname "$0")/helper_sdk_lib.sh"

CONTROL_SOCKET="${CONTROL_SOCKET:-/state/tx-generator-control.sock}"
FANOUT="${TX_GEN_FANOUT:-6}"
PROB_FRESH="${TX_GEN_PROB_FRESH:-0.5}"

# Antithesis exposes a uint64 randomness primitive via the
# fallback SDK's randomness source. Outside Antithesis we
# fall back to /dev/urandom for local-cluster testing.
read_seed() {
    if command -v antithesis_random >/dev/null 2>&1; then
        antithesis_random
    else
        # 8 bytes → big-endian uint64.
        od -An -tu8 -N8 /dev/urandom | tr -d ' \n'
    fi
}

SEED="$(read_seed)"
REQ="$(printf '{"transact":{"seed":%s,"fanout":%s,"prob_fresh":%s}}' \
    "$SEED" "$FANOUT" "$PROB_FRESH")"

sdk_reachable "tx_generator_transact_driver_started"

RSP="$(printf '%s\n' "$REQ" | nc -U -q 1 "$CONTROL_SOCKET")"

OK="$(printf '%s' "$RSP" | jq -r '.ok // false')"
REASON="$(printf '%s' "$RSP" | jq -r '.reason // ""')"

if [ "$OK" = "true" ]; then
    sdk_sometimes true "tx_generator_transact_landed"
    exit 0
fi

case "$REASON" in
    "no-pickable-source"|"index-not-ready"|"faucet-not-known")
        sdk_sometimes false "tx_generator_transact_not_applicable" \
            "$(jq -nc --arg r "$REASON" '{reason:$r}')"
        exit 1
        ;;
    submit-rejected:*)
        # Daemon's submit-rejected reason includes the raw
        # ledger error string, which can contain quotes and
        # backslashes. Build the details JSON via jq so the
        # SDK emitter never sees malformed input.
        sdk_unreachable "tx_generator_transact_submit_rejected" \
            "$(jq -nc --arg r "$REASON" '{reason:$r}')"
        exit 1
        ;;
    *)
        sdk_unreachable "tx_generator_transact_unknown_failure" \
            "$(jq -nc --arg r "$RSP" '{raw:$r}')"
        exit 1
        ;;
esac
