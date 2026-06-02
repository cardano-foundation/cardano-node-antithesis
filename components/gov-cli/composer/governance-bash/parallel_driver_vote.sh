#!/usr/bin/env bash
# parallel_driver_vote.sh — cast DRep + SPO + CC votes on one action.
#
# One logical cardano-cli governance operation = one driver. This one
# claims a pending InfoAction created by the create driver and submits
# YES votes from every DRep, every stake pool (SPO), and every
# constitutional committee member — mirroring conway_common.cast_vote /
# governance_setup._cast_vote from cardano-node-tests.

set -u
# shellcheck disable=SC1091
source "$(dirname "$0")/helper_sdk.sh"
# shellcheck disable=SC1091
source "$(dirname "$0")/helper_gov.sh"

sdk_reachable "vote entered"
mkdir -p "$WORK" "$STATE_DIR" "$ACTIONS_DIR"

if [ ! -f "$SETUP_MARKER" ]; then
    exit 0
fi
if ! wait_for_node 30; then
    sdk_unreachable "vote_node_not_ready"
    exit 0
fi

claim="$(pending_action)" || { gov_log "no pending action"; exit 0; }
read -r txid ix action_file <<<"$claim"
[ -n "$txid" ] || exit 0
tok="$(date +%s)_$$_${RANDOM}"
gov_log "voting on ${txid}#${ix}"

build_args=()
signing_args=()
witnesses=1   # faucet pays the fee

# DRep votes
for ((i = 1; i <= NUM_DREPS; i++)); do
    vf="$WORK/vote_drep${i}_${tok}.vote"
    cli conway governance vote create --yes \
        --governance-action-tx-id "$txid" \
        --governance-action-index "$ix" \
        --drep-verification-key-file "$GD/default_drep_${i}_drep.vkey" \
        --out-file "$vf" || continue
    build_args+=(--vote-file "$vf")
    signing_args+=(--signing-key-file "$GD/default_drep_${i}_drep.skey")
    witnesses=$((witnesses + 1))
done

# SPO (stake pool) votes — pool cold keys from the cardonnay genesis
for ((i = 1; i <= NUM_POOLS; i++)); do
    cold_v="$GOV/pools/node-pool${i}/cold.vkey"
    cold_s="$GOV/pools/node-pool${i}/cold.skey"
    [ -e "$cold_v" ] || continue
    vf="$WORK/vote_spo${i}_${tok}.vote"
    cli conway governance vote create --yes \
        --governance-action-tx-id "$txid" \
        --governance-action-index "$ix" \
        --cold-verification-key-file "$cold_v" \
        --out-file "$vf" || continue
    build_args+=(--vote-file "$vf")
    signing_args+=(--signing-key-file "$cold_s")
    witnesses=$((witnesses + 1))
done

# Constitutional committee votes — authorized hot keys
for ((i = 1; i <= NUM_CC; i++)); do
    hot_v="$GD/cc_member${i}_committee_hot.vkey"
    hot_s="$GD/cc_member${i}_committee_hot.skey"
    [ -e "$hot_v" ] || continue
    vf="$WORK/vote_cc${i}_${tok}.vote"
    cli conway governance vote create --yes \
        --governance-action-tx-id "$txid" \
        --governance-action-index "$ix" \
        --cc-hot-verification-key-file "$hot_v" \
        --out-file "$vf" || continue
    build_args+=(--vote-file "$vf")
    signing_args+=(--signing-key-file "$hot_s")
    witnesses=$((witnesses + 1))
done

if [ "${#build_args[@]}" -eq 0 ]; then
    sdk_sometimes false "votes_submitted"
    exit 0
fi

vtxid="$(sdk_run_signal_safe "vote_submit_signal" \
    build_sign_submit "vote_${tok}" 0 0 build_args signing_args)"

if [ -z "$vtxid" ]; then
    gov_log "vote submit failed"
    sdk_sometimes false "votes_submitted"
    exit 0
fi

# Verify the votes landed in gov-state.
prop="$(gov_state | jq -c --arg t "$txid" \
    '.proposals[]? | select(.actionId.txId==$t)' 2>/dev/null | head -1)"
has_drep="$(jq -r '(.dRepVotes // {}) | length' <<<"$prop" 2>/dev/null)"
has_spo="$(jq -r '(.stakePoolVotes // {}) | length' <<<"$prop" 2>/dev/null)"
has_cc="$(jq -r '(.committeeVotes // {}) | length' <<<"$prop" 2>/dev/null)"

sdk_sometimes "$([ "${has_drep:-0}" -ge 1 ] && echo true || echo false)" "drep_votes_recorded"
sdk_sometimes "$([ "${has_spo:-0}" -ge 1 ] && echo true || echo false)" "spo_votes_recorded"
sdk_sometimes "$([ "${has_cc:-0}" -ge 1 ] && echo true || echo false)" "cc_votes_recorded"
sdk_sometimes true "votes_submitted"

# Action handled — drop it from the queue (keep a voted/ record).
mv "$action_file" "$STATE_DIR/voted_${txid}.json" 2>/dev/null || true
gov_log "votes submitted: $vtxid (drep=$has_drep spo=$has_spo cc=$has_cc)"
exit 0
