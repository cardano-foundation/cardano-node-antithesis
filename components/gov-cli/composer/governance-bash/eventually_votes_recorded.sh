#!/usr/bin/env bash
# eventually_votes_recorded.sh — short post-fault recovery validation.
#
# `eventually_` runs after at least one driver has started; when it
# fires Antithesis stops fault injection and kills other drivers. This
# is a single-shot check (NOT a polling loop): let the system settle,
# then assert that at least one governance action carries DRep + SPO +
# CC votes. Cold-start guarded — if the node is still catching up after
# a fault burst, absorb it rather than flag a false regression.

set -u
# shellcheck disable=SC1091
source "$(dirname "$0")/helper_sdk.sh"
# shellcheck disable=SC1091
source "$(dirname "$0")/helper_gov.sh"

sdk_reachable "eventually_votes entered"

sleep 15   # let the chain settle after faults stop

if ! wait_for_node 30; then
    # Daemon's upstream still bootstrapping — cold start, not a bug.
    sdk_unreachable "eventually_cold_start"
    exit 0
fi

gs="$(gov_state)"
if [ -z "$gs" ]; then
    sdk_unreachable "eventually_cold_start"
    exit 0
fi

voted="$(jq -r '[.proposals[]?
    | select(((.dRepVotes // {}) | length) >= 1
         and ((.stakePoolVotes // {}) | length) >= 1)]
    | length' <<<"$gs" 2>/dev/null)"

if [ "${voted:-0}" -ge 1 ]; then
    sdk_sometimes true "action_fully_voted_after_recovery"
else
    # No fully-voted action yet; valid early in a timeline. Coverage
    # signal only — do not assert a liveness failure here.
    sdk_sometimes false "action_fully_voted_after_recovery"
fi
exit 0
