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

# Absorb signals delivered to the bash interpreter itself (SIGTERM /
# SIGINT / SIGPIPE) into a silent observation + exit 0. Antithesis
# can deliver any of these mid-script under fault injection; without
# this trap the script dies 128+sig and trips the composer's
# Always:zero-exit-code property. See #142.
sdk_install_signal_trap "stub heartbeat signal"

sdk_reachable "stub heartbeat entered"

# Multi-stage pipeline (printf | timeout 1 socat | jq) wrapped via
# sdk_run_signal_safe_fn so that signal-induced exits anywhere in
# the body get absorbed the same way single-binary launches in the
# sibling stubs already do. Function defined locally so it sees the
# outer set -u and locals.
# shellcheck disable=SC2329  # invoked indirectly by sdk_run_signal_safe_fn below
_heartbeat_body() {
    # `timeout 1 socat` bounds the indexer query; the outer wrapper
    # absorbs 124 if it fires.
    local reply
    reply="$(printf '{"ready": null}\n' \
              | timeout 1 socat - "UNIX-CONNECT:${INDEXER_SOCK}" 2>/dev/null \
              || true)"

    if [ -n "$reply" ] \
        && printf '%s' "$reply" \
            | jq -e '(.slotsBehind // null) != null and .slotsBehind <= 5' \
                >/dev/null 2>&1; then
        local processed tip behind
        processed="$(printf '%s' "$reply" | jq -r '.processedSlot // 0')"
        tip="$(printf '%s' "$reply"       | jq -r '.tipSlot // 0')"
        behind="$(printf '%s' "$reply"    | jq -r '.slotsBehind // 0')"
        sdk_sometimes true "stub heartbeat ticked" \
            "$(jq -nc --argjson p "$processed" --argjson t "$tip" --argjson b "$behind" \
                '{processedSlot:$p, tipSlot:$t, slotsBehind:$b}')"
    else
        sdk_sometimes false "stub heartbeat ticked" \
            "$(jq -nc --arg reply "$reply" \
                '{indexer_unresponsive:true, reply:$reply}')"
    fi
}

sdk_run_signal_safe_fn "stub heartbeat container_stopped" _heartbeat_body
exit 0
