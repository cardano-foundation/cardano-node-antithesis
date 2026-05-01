#!/usr/bin/env bash
# finally_adversary_summary.sh — diagnostic dump of the daemon's
# {"ready": null} snapshot at end-of-test.
#
# Always exits 0; the value is the JSON written into the SDK
# fallback file for inclusion in the Antithesis report.
#
# Wire schema:
# https://github.com/lambdasistemi/cardano-node-clients/blob/main/specs/036-cardano-adversary/contracts/control-wire.md

set -euo pipefail
SHELL="/bin/bash"
PATH="/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin:$PATH"

source "$(dirname "$0")/helper_sdk_lib.sh"

CONTROL_SOCKET="${CONTROL_SOCKET:-/state/adversary-control.sock}"

RSP="$(printf '{"ready": null}\n' | nc -U -N "$CONTROL_SOCKET" || echo '{}')"

sdk_reachable "adversary_summary" "$RSP"
exit 0
