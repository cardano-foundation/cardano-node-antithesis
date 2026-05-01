#!/usr/bin/env bash
# eventually_amaru_started.sh — post-fault proof that both relay-only
# Amaru nodes reached their `amaru run` exec point.

set -euo pipefail

# shellcheck disable=SC1091
source "$(dirname "$0")/helper_sdk_lib.sh"

STARTUP_DIR="${AMARU_STARTUP_DIR:-/amaru-startup}"
RELAYS="${AMARU_RELAYS:-amaru-relay-1 amaru-relay-2}"
MAX_ATTEMPTS="${MAX_ATTEMPTS:-60}"
RETRY_DELAY="${RETRY_DELAY:-2}"

array_json() {
    if [ "$#" -eq 0 ]; then
        printf '[]'
    else
        printf '%s\n' "$@" | jq -Rsc 'split("\n")[:-1]'
    fi
}

observed_markers() {
    local marker
    for marker in "$STARTUP_DIR"/*.started; do
        [ -e "$marker" ] || continue
        basename "$marker"
    done
}

sdk_reachable "eventually_amaru_started entered"

IFS=' ' read -r -a relay_names <<< "$RELAYS"

attempt=1
while [ "$attempt" -le "$MAX_ATTEMPTS" ]; do
    missing=()
    for relay in "${relay_names[@]}"; do
        if [ ! -f "$STARTUP_DIR/$relay.started" ]; then
            missing+=("$relay")
        fi
    done

    mapfile -t observed < <(observed_markers)
    observed_json="$(array_json "${observed[@]}")"
    missing_json="$(array_json "${missing[@]}")"
    details="$(jq -nc \
        --argjson attempt "$attempt" \
        --argjson observed "$observed_json" \
        --argjson missing "$missing_json" \
        '{attempt:$attempt, observed:$observed, missing:$missing}')"

    if [ "${#missing[@]}" -eq 0 ]; then
        sdk_sometimes true "amaru_relays_started" "$details"
        exit 0
    fi

    attempt=$((attempt + 1))
    sleep "$RETRY_DELAY"
done

sdk_unreachable "amaru_relays_not_started" "$details"
exit 1
