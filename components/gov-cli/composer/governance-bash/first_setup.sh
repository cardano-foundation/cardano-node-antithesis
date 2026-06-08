#!/usr/bin/env bash
# first_setup.sh — one-shot governance setup phase.
#
# `first_` commands run after `setup_complete`, before any driver, with
# NO fault injection. That makes this the right home for deterministic
# bootstrap: bring the committee + DReps on-chain so the create/vote
# drivers have something real to vote with.
#
# What it submits (mirrors cardonnay's register_entities_in_conway,
# minus pool re-registration — the pools are already registered SPOs
# from the create-staked genesis):
#   * CC hot-key authorization certs   -> committee can vote
#   * DRep registration certs          -> DReps exist
#   * vote-stake reg + vote-delegation -> DReps gain voting power
#     (each vote-stake address funded with DREP_DELEGATED lovelace)
# Then it waits one epoch so the DRep stake distribution goes live.

set -u
# shellcheck disable=SC1091
source "$(dirname "$0")/helper_sdk.sh"
# shellcheck disable=SC1091
source "$(dirname "$0")/helper_gov.sh"

readonly DREP_DELEGATED=500000000000

sdk_reachable "first_setup entered"
mkdir -p "$WORK" "$STATE_DIR"

if [ -f "$SETUP_MARKER" ]; then
    gov_log "setup already done"
    sdk_sometimes true "governance_setup_already_done"
    exit 0
fi

if ! wait_for_node; then
    gov_log "node not ready"
    sdk_unreachable "setup_node_not_ready"
    exit 1
fi

# Assemble the combined registration tx.
build_args=()
signing_args=()
witnesses=1   # faucet key

for ((i = 1; i <= NUM_DREPS; i++)); do
    build_args+=(
        --tx-out "$(cat "$GD/vote_stake_addr${i}.addr")+${DREP_DELEGATED}"
        --certificate-file "$GD/default_drep_${i}_drep_reg.cert"
        --certificate-file "$GD/vote_stake_addr${i}_stake.reg.cert"
        --certificate-file "$GD/vote_stake_addr${i}_stake.vote_deleg.cert"
    )
    signing_args+=(
        --signing-key-file "$GD/default_drep_${i}_drep.skey"
        --signing-key-file "$GD/vote_stake_addr${i}.skey"
        --signing-key-file "$GD/vote_stake_addr${i}_stake.skey"
    )
    witnesses=$((witnesses + 3))
done

for ((i = 1; i <= NUM_CC; i++)); do
    auth="$GD/cc_member${i}_committee_hot_auth.cert"
    [ -e "$auth" ] || continue
    build_args+=(--certificate-file "$auth")
    signing_args+=(--signing-key-file "$GD/cc_member${i}_committee_cold.skey")
    witnesses=$((witnesses + 1))
done

# Deposits: each DRep pays a stake-key deposit + a DRep deposit; CC
# hot-auth certs carry no deposit. Funding outputs are in build_args.
sdep="$(stake_deposit)"; ddep="$(drep_deposit)"
txout_total=$(( NUM_DREPS * DREP_DELEGATED ))
deposit_total=$(( NUM_DREPS * (sdep + ddep) ))
txid="$(build_sign_submit "setup_register" "$txout_total" "$deposit_total" build_args signing_args)"
if [ -z "$txid" ]; then
    gov_log "registration tx failed"
    sdk_unreachable "setup_registration_failed"
    exit 1
fi
gov_log "registration tx submitted: $txid"
sdk_sometimes true "governance_registration_submitted"

# DRep stake delegation becomes effective at the next epoch boundary.
start_epoch="$(current_epoch)"
if [ -n "$start_epoch" ]; then
    # Epochs are ~16.7 min (epochLength 5000 × 0.2s), so allow > 1 epoch.
    wait_for_epoch "$((start_epoch + 1))" 1800 || gov_log "epoch wait timed out (continuing)"
fi

# Confirm the committee came up Active and a DRep is visible.
cc_active="$(cli conway query committee-state --active "${MAGIC[@]}" 2>/dev/null \
    | grep -c '"status": "Active"')"
sdk_sometimes "$([ "${cc_active:-0}" -ge 1 ] && echo true || echo false)" \
    "committee_active_after_setup"

drep_seen="$(cli conway query drep-state --all-dreps "${MAGIC[@]}" 2>/dev/null | jq -r 'length // 0' 2>/dev/null)"
sdk_sometimes "$([ "${drep_seen:-0}" -ge 1 ] && echo true || echo false)" \
    "dreps_registered_after_setup"

touch "$SETUP_MARKER"
sdk_sometimes true "governance_setup_complete"
gov_log "setup complete (cc_active=$cc_active dreps=$drep_seen)"
exit 0
