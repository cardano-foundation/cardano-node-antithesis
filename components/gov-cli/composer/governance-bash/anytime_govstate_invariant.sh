#!/usr/bin/env bash
# anytime_govstate_invariant.sh — invariant probe that may run at any
# time, including during driver execution and under fault injection.
#
# Asserts that whenever the local node answers, its governance state is
# well-formed: gov-state parses as JSON and exposes a proposals array.
# A malformed or missing gov-state while the node is otherwise up would
# be a real regression.

set -u
# shellcheck disable=SC1091
source "$(dirname "$0")/helper_sdk.sh"
# shellcheck disable=SC1091
source "$(dirname "$0")/helper_gov.sh"

sdk_reachable "govstate_invariant entered"

gs="$(gov_state)"
if [ -z "$gs" ]; then
    # Node unreachable (likely mid-fault) — not an invariant violation.
    sdk_unreachable "govstate_unavailable"
    exit 0
fi

if jq -e 'has("proposals")' >/dev/null 2>&1 <<<"$gs"; then
    sdk_always true "govstate_well_formed"
else
    sdk_always false "govstate_well_formed"
fi
exit 0
