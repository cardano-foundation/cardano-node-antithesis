#!/usr/bin/env bash
# finally_amaru_started.sh - final proof that both relay-only Amaru
# nodes reached their `amaru run` exec point after consuming the
# bootstrap bundle.

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

probe_startup_markers() {
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
}

sdk_reachable "finally_amaru_started entered"

IFS=' ' read -r -a relay_names <<< "$RELAYS"

details='{"attempt":0,"observed":[],"missing":[]}'
attempt=1
while [ "$attempt" -le "$MAX_ATTEMPTS" ]; do
    probe_startup_markers
    if [ "${#missing[@]}" -eq 0 ]; then
        sdk_sometimes true "amaru_relays_started" "$details"
        sdk_always true "amaru_relays_started_at_final_check" "$details"
        exit 0
    fi

    attempt=$((attempt + 1))
    sleep "$RETRY_DELAY"
done

sdk_always false "amaru_relays_started_at_final_check" "$details"
sdk_unreachable "amaru_relays_not_started" "$details"
exit 1
