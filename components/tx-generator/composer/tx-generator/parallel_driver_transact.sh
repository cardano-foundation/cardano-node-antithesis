#!/usr/bin/env bash
# parallel_driver_transact.sh — fire one transact request
# at the cardano-tx-generator daemon. Reads an Antithesis
# random uint64 (or falls back to /dev/urandom outside the
# Antithesis runtime) and sends a single NDJSON request
# over the daemon's control socket.
#
# Always exits 0. Tick state is encoded purely via SDK
# assertions (Reachability for documented states,
# Unreachability for the strict invariants), matching
# components/asteria-stub/composer/stub/parallel_driver_heartbeat.sh.
# Antithesis's built-in 'Always: Commands finish with
# zero exit code' property has no opt-out and any non-zero
# exit fails it.
#
# Wire schema lives in
# https://github.com/lambdasistemi/cardano-node-clients/blob/main/specs/034-cardano-tx-generator/contracts/control-wire.md

set -u
SHELL="/bin/bash"
PATH="/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin:$PATH"

# NOTE: deliberately NOT 'set -e'. The composer fires
# this script before the daemon's control socket is
# bound during early boot; nc returns non-zero on
# refused connection, and 'set -e' would kill the
# script before any exit-0 path runs.

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

RSP="$(printf '%s\n' "$REQ" | nc -U -q 1 "$CONTROL_SOCKET" 2>/dev/null || true)"

# Empty RSP means the control socket isn't connectable
# right now (daemon booting, or the supervisor briefly
# tearing down the listener). That's a reachability
# event, not a failure — exit 0 so the composer's
# "Always exit 0" property holds and the next tick
# retries.
if [ -z "$RSP" ]; then
    sdk_reachable "tx_generator_transact_daemon_unreachable"
    exit 0
fi

OK="$(printf '%s' "$RSP" | jq -r '.ok // false' 2>/dev/null || echo false)"
REASON="$(printf '%s' "$RSP" | jq -r '.reason // ""' 2>/dev/null || echo "")"

if [ "$OK" = "true" ]; then
    sdk_sometimes true "tx_generator_transact_landed"
    exit 0
fi

case "$REASON" in
    "no-pickable-source"|"index-not-ready"|"faucet-not-known")
        # Documented not-applicable states; Reachability
        # accumulates samples without a pass/fail grade.
        # Antithesis grades a Sometimes assertion as
        # PASSING when at least one sample has
        # condition=true; we always emit condition=false
        # for not-applicable, so that type can never
        # satisfy.
        sdk_reachable "tx_generator_transact_not_applicable" \
            "$(jq -nc --arg r "$REASON" '{reason:$r}')"
        exit 0
        ;;
    submit-rejected:*)
        # Daemon's submit-rejected reason includes the raw
        # ledger error string, which can contain quotes
        # and backslashes. Build the details JSON via jq
        # so the SDK emitter never sees malformed input.
        # Strict (Unreachability) — daemon-side fixes
        # (#105/#110/#114) are expected to drive this to
        # zero.
        sdk_unreachable "tx_generator_transact_submit_rejected" \
            "$(jq -nc --arg r "$REASON" '{reason:$r}')"
        exit 0
        ;;
    *)
        sdk_unreachable "tx_generator_transact_unknown_failure" \
            "$(jq -nc --arg r "$RSP" '{raw:$r}')"
        exit 0
        ;;
esac
