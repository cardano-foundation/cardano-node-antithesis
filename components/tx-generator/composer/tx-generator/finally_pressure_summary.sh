#!/usr/bin/env bash
# finally_pressure_summary.sh — diagnostic dump of the
# daemon's snapshot at end-of-test. Always exits 0; the
# value is the JSON written into the SDK fallback file
# for inclusion in the Antithesis report.

set -u
SHELL="/bin/bash"
PATH="/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin:$PATH"

source "$(dirname "$0")/helper_sdk_lib.sh"

CONTROL_SOCKET="${CONTROL_SOCKET:-/state/tx-generator-control.sock}"

RSP="$(control_socket_request "$CONTROL_SOCKET" '{"snapshot":null}')"

if [ -z "$RSP" ]; then
    RSP="{}"
elif ! printf '%s' "$RSP" | jq -e . >/dev/null 2>&1; then
    RSP="$(jq -nc --arg raw "$RSP" \
        '{raw:$raw, reason:"summary probe returned invalid JSON"}')"
fi

sdk_reachable "tx_generator_pressure_summary" "$RSP"
exit 0
