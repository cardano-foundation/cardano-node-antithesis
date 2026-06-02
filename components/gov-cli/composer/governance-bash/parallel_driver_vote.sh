#!/usr/bin/env bash
# parallel_driver_vote.sh — cast DRep + SPO + CC votes on one action.
#
# One logical cardano-cli governance operation = one driver. This one
# picks an InfoAction from the create driver's published set (created.log
# MINUS rejected.log) and submits YES votes from every DRep, every stake
# pool (SPO), and every constitutional committee member — mirroring
# conway_common.cast_vote / governance_setup._cast_vote from
# cardano-node-tests. A vote that is rejected retires the action.

set -u
# shellcheck disable=SC1091
source "$(dirname "$0")/helper_sdk.sh"
# shellcheck disable=SC1091
source "$(dirname "$0")/helper_gov.sh"

sdk_reachable "vote entered"
mkdir -p "$WORK" "$STATE_DIR"

if [ ! -f "$SETUP_MARKER" ]; then
    exit 0
fi
if ! wait_for_node 30; then
    sdk_unreachable "vote_node_not_ready"
    exit 0
fi

# Work the set difference (created MINUS rejected) and let Antithesis'
# RNG steer every choice: which action, which voter, and yes/no. One
# invocation casts ONE voter's single vote, so the hypervisor explores a
# fine-grained space of (action, voter, decision) combinations.
mapfile -t pend < <(pending_actions)
if [ "${#pend[@]}" -eq 0 ]; then
    gov_log "no pending actions"
    exit 0
fi

# RNG-select one action from the live set.
read -r txid ix _ <<<"${pend[$(rng_mod "${#pend[@]}")]}"
[ -n "$txid" ] || exit 0

# Retire it if it is no longer on-chain (expired/decided); another
# invocation will pick a different one.
if ! action_onchain "$txid"; then
    gov_log "action $txid no longer on-chain; retiring"
    record_rejected "$txid"
    sdk_sometimes true "stale_action_retired"
    exit 0
fi

# Build the voter roster (every DRep, SPO and CC member), then RNG-select
# who votes this time. Each entry: kind|vkey-flag|vkey-file|skey-file.
voters=()
for ((i = 1; i <= NUM_DREPS; i++)); do
    voters+=("drep|--drep-verification-key-file|$GD/default_drep_${i}_drep.vkey|$GD/default_drep_${i}_drep.skey")
done
for ((i = 1; i <= NUM_POOLS; i++)); do
    cv="$GOV/pools/node-pool${i}/cold.vkey"
    [ -e "$cv" ] && voters+=("spo|--cold-verification-key-file|$cv|$GOV/pools/node-pool${i}/cold.skey")
done
for ((i = 1; i <= NUM_CC; i++)); do
    hv="$GD/cc_member${i}_committee_hot.vkey"
    [ -e "$hv" ] && voters+=("cc|--cc-hot-verification-key-file|$hv|$GD/cc_member${i}_committee_hot.skey")
done
[ "${#voters[@]}" -gt 0 ] || { sdk_sometimes false "votes_submitted"; exit 0; }

IFS='|' read -r kind vkey_flag vkey skey <<<"${voters[$(rng_mod "${#voters[@]}")]}"

# RNG-select the decision: yes, no or abstain.
case "$(rng_mod 3)" in
    0) choice="--yes";     decision="yes" ;;
    1) choice="--no";      decision="no" ;;
    *) choice="--abstain"; decision="abstain" ;;
esac

tok="$(date +%s)_$$_${RANDOM}"
gov_log "voting ${decision} as ${kind} on ${txid}#${ix}"

vf="$WORK/vote_${kind}_${tok}.vote"
if ! cli conway governance vote create "$choice" \
    --governance-action-tx-id "$txid" \
    --governance-action-index "$ix" \
    "$vkey_flag" "$vkey" \
    --out-file "$vf"; then
    sdk_sometimes false "votes_submitted"
    exit 0
fi

build_args=(--vote-file "$vf")
signing_args=(--signing-key-file "$skey")

vtxid="$(sdk_run_signal_safe "vote_submit_signal" \
    build_sign_submit "vote_${tok}" 0 0 build_args signing_args)"

if [ -z "$vtxid" ]; then
    # Vote rejected (expired / already decided / invalid) — retire the
    # action so the created-minus-rejected difference stops offering it.
    gov_log "vote rejected for $txid; retiring"
    record_rejected "$txid"
    sdk_sometimes false "votes_submitted"
    exit 0
fi

# Verify the vote landed and emit per-kind / per-decision coverage.
prop="$(gov_state | jq -c --arg t "$txid" \
    '.proposals[]? | select(.actionId.txId==$t)' 2>/dev/null | head -1)"
total="$(jq -r '((.dRepVotes // {})|length) + ((.stakePoolVotes // {})|length) + ((.committeeVotes // {})|length)' \
    <<<"$prop" 2>/dev/null)"
sdk_sometimes "$([ "${total:-0}" -ge 1 ] && echo true || echo false)" "vote_recorded_${kind}"
sdk_sometimes true "vote_decision_${decision}"
sdk_sometimes true "votes_submitted"

gov_log "vote submitted: $vtxid (${kind} ${decision}; action now has ${total} votes)"
exit 0
