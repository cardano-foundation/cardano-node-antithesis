#!/usr/bin/env bash
# parallel_driver_heartbeat.sh — query the long-lived utxo-indexer.
#
# Runs concurrently with fault injection. Each invocation pings the
# indexer's `ready` endpoint over its Unix domain socket and emits a
# Sometimes assertion proving the indexer is responsive and tracking
# the chain.
#
# Exit 0 always — failure of this driver is a Sometimes-false signal,
# not a process-level error.

set -u

# shellcheck disable=SC1091
source "$(dirname "$0")/helper_sdk.sh"

INDEXER_SOCK="${INDEXER_SOCK:-/tmp/idx.sock}"

sdk_reachable "stub heartbeat entered"

# `timeout 1 socat` bounds the heartbeat under composer's
# parallel_driver per-command cap (~16 s).
REPLY="$(printf '{"ready": null}\n' | timeout 1 socat - "UNIX-CONNECT:${INDEXER_SOCK}" 2>/dev/null || true)"

if [ -n "$REPLY" ] && printf '%s' "$REPLY" | jq -e '(.slotsBehind // null) != null and .slotsBehind <= 5' >/dev/null 2>&1; then
    PROCESSED="$(printf '%s' "$REPLY" | jq -r '.processedSlot // 0')"
    TIP="$(printf '%s' "$REPLY" | jq -r '.tipSlot // 0')"
    BEHIND="$(printf '%s' "$REPLY" | jq -r '.slotsBehind // 0')"
    sdk_sometimes true "stub heartbeat ticked" \
        "$(jq -nc --argjson p "$PROCESSED" --argjson t "$TIP" --argjson b "$BEHIND" \
            '{processedSlot:$p, tipSlot:$t, slotsBehind:$b}')"
else
    sdk_sometimes false "stub heartbeat ticked" \
        "$(jq -nc --arg reply "$REPLY" '{indexer_unresponsive:true, reply:$reply}')"
fi

exit 0
