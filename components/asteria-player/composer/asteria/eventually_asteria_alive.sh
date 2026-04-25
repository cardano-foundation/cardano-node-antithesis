#!/usr/bin/env bash
# eventually_asteria_alive.sh — confirms the asteria players are
# emitting SDK events into the fallback file within a bounded
# window. Iteration 1: passes if the bootstrap event has appeared
# within MAX_ATTEMPTS * RETRY_DELAY seconds.

set -euo pipefail
SHELL="/bin/bash"
PATH="/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin:$PATH"

source "$(dirname "$0")/helper_sdk_lib.sh"

MAX_ATTEMPTS="${MAX_ATTEMPTS:-30}"
RETRY_DELAY="${RETRY_DELAY:-2}"
SDK_FILE="${ANTITHESIS_OUTPUT_DIR:-/tmp}/sdk.jsonl"

sdk_reachable "eventually_asteria_alive_started"

attempt=0
while [ "$attempt" -lt "$MAX_ATTEMPTS" ]; do
    if [ -f "$SDK_FILE" ] && grep -q "asteria_bootstrap_completed" "$SDK_FILE"; then
        sdk_sometimes true "asteria_bootstrap_observed"
        exit 0
    fi
    attempt=$((attempt + 1))
    sleep "$RETRY_DELAY"
done

sdk_unreachable "eventually_asteria_alive_timeout" \
    "{\"max_attempts\":$MAX_ATTEMPTS,\"retry_delay\":$RETRY_DELAY}"
exit 1
