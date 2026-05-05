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

if [ "${TX_GEN_SUMMARY_DEADLINE_WRAPPED:-0}" != "1" ]; then
    TX_GEN_SUMMARY_DEADLINE_WRAPPED=1 \
        timeout --kill-after=2s 12s "$0" "$@"
    rc=$?
    case "$rc" in
        0)
            exit 0
            ;;
        124 | 129 | 137 | 143 | 255)
            sdk_reachable "tx_generator_pressure_summary_deadline" \
                "$(jq -nc --argjson rc "$rc" \
                    '{rc:$rc, reason:"summary probe exceeded its safety deadline or was interrupted by a container fault"}')"
            exit 0
            ;;
        *)
            sdk_unreachable "tx_generator_pressure_summary_unexpected_exit" \
                "$(jq -nc --argjson rc "$rc" \
                    '{rc:$rc, reason:"summary probe returned non-zero outside the signal/deadline absorption set"}')"
            exit 0
            ;;
    esac
fi

RSP="$(timeout --kill-after=2s 5s sh -c \
    "printf '{\"snapshot\":null}\\n' | nc -U -q 1 '$CONTROL_SOCKET'" \
    2>/dev/null || true)"

if [ -z "$RSP" ]; then
    RSP="{}"
elif ! printf '%s' "$RSP" | jq -e . >/dev/null 2>&1; then
    RSP="$(jq -nc --arg raw "$RSP" \
        '{raw:$raw, reason:"summary probe returned invalid JSON"}')"
fi

sdk_reachable "tx_generator_pressure_summary" "$RSP"
exit 0
