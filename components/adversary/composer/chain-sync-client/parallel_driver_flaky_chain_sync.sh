#!/usr/bin/env bash
# parallel_driver_flaky_chain_sync.sh — fire one
# chain_sync_flap request at the cardano-adversary daemon.
#
# Reads an Antithesis random uint64 (or falls back to
# /dev/urandom outside the Antithesis runtime) and sends a
# single NDJSON request over the daemon's control socket.
# Maps the structured response onto SDK assertions per the
# table in the wire spec.
#
# Wire schema:
# https://github.com/lambdasistemi/cardano-node-clients/blob/main/specs/036-cardano-adversary/contracts/control-wire.md

set -euo pipefail
SHELL="/bin/bash"
PATH="/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin:$PATH"

source "$(dirname "$0")/helper_sdk_lib.sh"

CONTROL_SOCKET="${CONTROL_SOCKET:-/state/adversary-control.sock}"
LIMIT="${LIMIT:-100}"
NCONNS="${NCONNS:-1}"

sdk_reachable "adversary_chain_sync_flap_started"

read_seed() {
    if command -v antithesis_random >/dev/null 2>&1; then
        antithesis_random
    else
        # 8 bytes → big-endian uint64.
        od -An -tu8 -N8 /dev/urandom | tr -d ' \n'
    fi
}

SEED="$(read_seed)"
REQ="$(printf \
    '{"chain_sync_flap":{"seed":%s,"limit":%s,"n_conns":%s}}' \
    "$SEED" "$LIMIT" "$NCONNS")"

# OpenBSD nc is what dockerTools.buildImage ships
# (pkgs.netcat-openbsd in the cardano-adversary image).
# `-N` shuts the network socket on stdin EOF so the daemon's
# single-line response is fully read; `-q` is GNU-only and
# was the wrong flag in the v1 driver.
RSP="$(printf '%s\n' "$REQ" | nc -U -N "$CONTROL_SOCKET" || true)"

if [ -z "$RSP" ]; then
    sdk_unreachable "adversary_unexpected_response" \
        '{"reason":"empty-or-failed-recv"}'
    exit 1
fi

OK="$(printf '%s' "$RSP" | jq -r '.ok // false')"
REASON="$(printf '%s' "$RSP" | jq -r '.reason // ""')"
DETAILS_JSON="$(printf '%s' "$RSP" | jq -c '.')"

if [ "$OK" = "true" ]; then
    sdk_sometimes true "adversary_chain_sync_flap_completed" \
        "$DETAILS_JSON"
    exit 0
fi

case "$REASON" in
    no-chain-points-yet)
        # Expected at start-of-test before tracer-sidecar emits
        # any points. Treat as a skipped tick rather than a
        # failure — Antithesis records "command not applicable
        # this iteration" and the report's Sometimes counter
        # captures how often it happened.
        sdk_sometimes false "adversary_no_chain_points" \
            "$DETAILS_JSON"
        exit 1
        ;;
    no-chain-points-file)
        # Misconfigured compose: --chain-points-file missing or
        # path doesn't exist. Should never be reached under
        # normal operation.
        sdk_unreachable "adversary_misconfigured_no_points_file" \
            "$DETAILS_JSON"
        exit 1
        ;;
    no-producers)
        # Misconfigured compose: no --producer-host. Should
        # never be reached under normal operation.
        sdk_unreachable "adversary_misconfigured_no_producers" \
            "$DETAILS_JSON"
        exit 1
        ;;
    not-implemented)
        # Should never appear post-PR-C. If it does, the daemon
        # image is older than expected.
        sdk_unreachable "adversary_endpoint_not_implemented" \
            "$DETAILS_JSON"
        exit 1
        ;;
    *)
        sdk_unreachable "adversary_unexpected_response" \
            "$DETAILS_JSON"
        exit 1
        ;;
esac
