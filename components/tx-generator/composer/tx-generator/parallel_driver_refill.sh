#!/usr/bin/env bash
# parallel_driver_refill.sh — fire one refill request at
# the cardano-tx-generator daemon. Identical shape to
# parallel_driver_transact.sh; lower composer weight so
# refills fire less often than transactions.

set -euo pipefail
SHELL="/bin/bash"
PATH="/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin:$PATH"

source "$(dirname "$0")/helper_sdk_lib.sh"

CONTROL_SOCKET="${CONTROL_SOCKET:-/state/tx-generator-control.sock}"

read_seed() {
    if command -v antithesis_random >/dev/null 2>&1; then
        antithesis_random
    else
        od -An -tu8 -N8 /dev/urandom | tr -d ' \n'
    fi
}

SEED="$(read_seed)"
REQ="$(printf '{"refill":{"seed":%s}}' "$SEED")"

sdk_reachable "tx_generator_refill_driver_started"

RSP="$(printf '%s\n' "$REQ" | nc -U -q 1 "$CONTROL_SOCKET")"

OK="$(printf '%s' "$RSP" | jq -r '.ok // false')"
REASON="$(printf '%s' "$RSP" | jq -r '.reason // ""')"

if [ "$OK" = "true" ]; then
    sdk_sometimes true "tx_generator_refill_landed"
    exit 0
fi

case "$REASON" in
    "faucet-not-known"|"faucet-exhausted")
        sdk_sometimes false "tx_generator_refill_not_applicable" \
            "$(jq -nc --arg r "$REASON" '{reason:$r}')"
        exit 1
        ;;
    submit-rejected:*)
        # Daemon's submit-rejected reason includes the raw
        # ledger error string, which can contain quotes and
        # backslashes. Build the details JSON via jq so the
        # SDK emitter never sees malformed input.
        sdk_unreachable "tx_generator_refill_submit_rejected" \
            "$(jq -nc --arg r "$REASON" '{reason:$r}')"
        exit 1
        ;;
    *)
        sdk_unreachable "tx_generator_refill_unknown_failure" \
            "$(jq -nc --arg r "$RSP" '{raw:$r}')"
        exit 1
        ;;
esac
