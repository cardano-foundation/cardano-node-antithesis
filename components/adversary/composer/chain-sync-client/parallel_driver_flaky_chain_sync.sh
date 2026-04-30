#!/usr/bin/env bash
# parallel_driver_flaky_chain_sync.sh — fire one
# chain_sync_flap request at the cardano-adversary daemon.
# Reads an Antithesis random uint64 (or falls back to
# /dev/urandom outside the Antithesis runtime) and sends a
# single NDJSON request over the daemon's control socket.
#
# Wire schema:
# https://github.com/lambdasistemi/cardano-node-clients/blob/main/specs/036-cardano-adversary/contracts/control-wire.md

set -euo pipefail
SHELL="/bin/bash"
PATH="/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin:$PATH"

CONTROL_SOCKET="${CONTROL_SOCKET:-/state/adversary-control.sock}"
LIMIT="${LIMIT:-100}"
NCONNS="${NCONNS:-1}"

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

# nc -U speaks Unix socket; -q 1 closes 1s after EOF on stdin
# so the daemon's response body is fully read.
RSP="$(printf '%s\n' "$REQ" | nc -U -q 1 "$CONTROL_SOCKET")"

OK="$(printf '%s' "$RSP" | jq -r '.ok // false')"
REASON="$(printf '%s' "$RSP" | jq -r '.reason // ""')"

if [ "$OK" = "true" ]; then
    exit 0
fi

case "$REASON" in
    no-chain-points-yet)
        # Expected at start-of-test before tracer-sidecar emits
        # any points. Treat as a skipped tick rather than a
        # failure so Antithesis records "command not applicable
        # this iteration".
        exit 1
        ;;
    no-chain-points-file | no-producers)
        # Configuration error in the docker-compose. Surface
        # loudly so the operator notices.
        echo "::error::adversary daemon misconfigured: ${REASON}" >&2
        exit 1
        ;;
    not-implemented)
        # Should never appear post-PR-C. If it does, the daemon
        # image is older than expected.
        echo "::error::adversary daemon is too old (chain_sync_flap not implemented)" >&2
        exit 1
        ;;
    *)
        echo "::error::unexpected adversary response: ${RSP}" >&2
        exit 1
        ;;
esac
