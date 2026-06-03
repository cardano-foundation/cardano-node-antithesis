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

# Invariant: once setup has authorized the committee, its quorum must
# survive fault injection (CC auth is on-chain state, not a container, so
# killing producers must never drop authorized members below minSize).
if [ -f "$SETUP_MARKER" ]; then
    auth="$(cli conway query committee-state --active "${MAGIC[@]}" 2>/dev/null \
        | grep -c '"status": "Active"')"
    if [ -n "$auth" ] && [ "$auth" -ge 0 ] 2>/dev/null; then
        # committeeMinSize is 2 in the seeded Conway genesis.
        sdk_always "$([ "$auth" -ge 2 ] && echo true || echo false)" \
            "committee_quorum_maintained" \
            "$(jq -nc --argjson a "$auth" '{authorized:$a, min:2}')"
    fi
fi
exit 0
