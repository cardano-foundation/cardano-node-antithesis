#!/usr/bin/env bash
# eventually_adversary_active.sh — short single-shot validator
# (≤1 min total per the antithesis-tests skill rule).
#
# Sleeps the post-fault settle window, asks the daemon
# {"ready": null} once over the control socket, asserts ready=true.
# Emits sdk_sometimes true on success, sdk_sometimes false otherwise.
#
# Wire schema:
# https://github.com/lambdasistemi/cardano-node-clients/blob/main/specs/036-cardano-adversary/contracts/control-wire.md

set -euo pipefail
SHELL="/bin/bash"
PATH="/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin:$PATH"

source "$(dirname "$0")/helper_sdk_lib.sh"

CONTROL_SOCKET="${CONTROL_SOCKET:-/state/adversary-control.sock}"

sdk_reachable "adversary_eventually_started"

# Settle window — the daemon may have been killed by Antithesis
# fault-injection mid-request and need a moment to rebind its
# control socket on restart.
sleep 15

RSP="$(printf '{"ready": null}\n' | nc -U -N "$CONTROL_SOCKET" || true)"

if [ -z "$RSP" ]; then
    sdk_sometimes false "adversary_not_ready_after_settle" \
        '{"reason":"empty-or-failed-recv"}'
    exit 1
fi

READY="$(printf '%s' "$RSP" | jq -r '.ready // false')"
DETAILS_JSON="$(printf '%s' "$RSP" | jq -c '.')"

if [ "$READY" = "true" ]; then
    sdk_sometimes true "adversary_ready" "$DETAILS_JSON"
    exit 0
fi

sdk_sometimes false "adversary_not_ready_after_settle" "$DETAILS_JSON"
exit 1
