#!/usr/bin/env bash
# eventually_pressure_summary.sh — diagnostic dump of the
# daemon's snapshot at end-of-workload. Always exits 0; the
# value is the JSON written into the SDK fallback file
# for inclusion in the Antithesis report.
#
# Was finally_pressure_summary.sh. Moved to the eventually_ category
# because finally_ commands stopped running to completion at config
# version cfg=56 (Antithesis masks the finally_ set to weight 0,
# tripping "All commands were run to completion at least once").
# eventually_ commands complete reliably here. The probe is a single
# fast control-socket round trip, so it sits well inside the
# eventually_ per-command cap. The tx-generator daemon is a container
# service (not a composer command), so it is still up after eventually_
# kills the drivers and the snapshot remains meaningful.

set -u
SHELL="/bin/bash"
PATH="/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin:$PATH"

source "$(dirname "$0")/helper_sdk_lib.sh"
sdk_install_signal_trap "tx_generator_pressure_signal"

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
