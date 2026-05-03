#!/usr/bin/env bash
# parallel_driver_refill.sh — fire one refill request at
# the cardano-tx-generator daemon. Identical shape to
# parallel_driver_transact.sh; lower composer weight so
# refills fire less often than transactions.
#
# Always exits 0. See parallel_driver_transact.sh for
# the rationale (asteria-stub convention; Antithesis's
# 'Always exit 0' built-in property has no opt-out).

set -u
SHELL="/bin/bash"
PATH="/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin:$PATH"

# See parallel_driver_transact.sh for why we don't
# 'set -e' here.

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

# Hard 5s wall-clock on the request/response. Without this the
# kernel's nc behaviour can leave the script blocked for >25s when
# the daemon's accept loop is wedged mid-reconnect under a fault
# window — long enough for the composer's per-step deadline to kill
# us, surfacing as a non-zero exit on the built-in
# 'Commands finish with zero exit code' property. Treat any timeout
# the same as the empty-response path: fire the unreachable
# Reachability marker and exit 0.
RSP="$(timeout --kill-after=2s 5s sh -c \
    "printf '%s\\n' '$REQ' | nc -U -q 1 '$CONTROL_SOCKET'" \
    2>/dev/null || true)"

if [ -z "$RSP" ]; then
    sdk_reachable "tx_generator_refill_daemon_unreachable"
    exit 0
fi

OK="$(printf '%s' "$RSP" | jq -r '.ok // false' 2>/dev/null || echo false)"
REASON="$(printf '%s' "$RSP" | jq -r '.reason // ""' 2>/dev/null || echo "")"

if [ "$OK" = "true" ]; then
    sdk_sometimes true "tx_generator_refill_landed"
    exit 0
fi

case "$REASON" in
    "faucet-not-known"|"faucet-exhausted"|"index-not-ready")
        # 'index-not-ready' arrives from the daemon's
        # post-reconnect freshness gate (cardano-node-clients#110)
        # and pre-submit chain-tip probe (#114). Refill
        # arms back off until the indexer has a fresh view;
        # Reachability is the right shape.
        sdk_reachable "tx_generator_refill_not_applicable" \
            "$(jq -nc --arg r "$REASON" '{reason:$r}')"
        exit 0
        ;;
    submit-rejected:*)
        sdk_unreachable "tx_generator_refill_submit_rejected" \
            "$(jq -nc --arg r "$REASON" '{reason:$r}')"
        exit 0
        ;;
    *)
        sdk_unreachable "tx_generator_refill_unknown_failure" \
            "$(jq -nc --arg r "$RSP" '{raw:$r}')"
        exit 0
        ;;
esac
