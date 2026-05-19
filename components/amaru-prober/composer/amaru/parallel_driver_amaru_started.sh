#!/usr/bin/env bash
# parallel_driver_amaru_started.sh - cheap anytime probe for Amaru
# relay startup markers. This command always exits 0; the final proof
# lives in finally_amaru_started.sh.

set -euo pipefail

# shellcheck disable=SC1091
source "$(dirname "$0")/helper_sdk_lib.sh"

STARTUP_DIR="${AMARU_STARTUP_DIR:-/amaru-startup}"
RELAYS="${AMARU_RELAYS:-amaru-relay-1 amaru-relay-2}"

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

IFS=' ' read -r -a relay_names <<< "$RELAYS"

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
    --argjson observed "$observed_json" \
    --argjson missing "$missing_json" \
    '{observed:$observed, missing:$missing}')"

sdk_reachable "amaru_startup_probe_entered"

if [ "${#missing[@]}" -eq 0 ]; then
    sdk_sometimes true "amaru_relays_started" "$details"
else
    sdk_reachable "amaru_relays_waiting_for_startup" "$details"
fi

exit 0
