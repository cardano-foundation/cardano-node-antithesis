#!/usr/bin/env bash
# parallel_driver_create_action.sh — submit one InfoAction.
#
# One logical cardano-cli governance operation = one driver. This one
# creates an "info" governance action (a no-op poll that never enacts,
# so the workload is unbounded under fault injection) and records its
# id so the vote driver can act on it.

set -u
# shellcheck disable=SC1091
source "$(dirname "$0")/helper_sdk.sh"
# shellcheck disable=SC1091
source "$(dirname "$0")/helper_gov.sh"

sdk_reachable "create_action entered"
mkdir -p "$WORK" "$STATE_DIR"

# Setup must have run; first_ runs before parallel drivers, but be safe.
if [ ! -f "$SETUP_MARKER" ]; then
    gov_log "setup not done yet"
    exit 0
fi
if ! wait_for_node 30; then
    sdk_unreachable "create_action_node_not_ready"
    exit 0
fi

tok="$(date +%s)_$$_${RANDOM}"
action_file="$WORK/info_${tok}.action"
deposit="$(gov_deposit)"
ah="$(anchor_hash)"
[ -n "$ah" ] || { sdk_unreachable "create_action_anchor_failed"; exit 0; }

# Info action; deposit returned to a vote-stake credential.
# `create-info` takes the network tag (--testnet), not --testnet-magic.
if ! cli conway governance action create-info \
    --testnet \
    --governance-action-deposit "$deposit" \
    --deposit-return-stake-verification-key-file "$GD/vote_stake_addr1_stake.vkey" \
    --anchor-url "$ANCHOR_URL" \
    --anchor-data-hash "$ah" \
    --out-file "$action_file"; then
    sdk_unreachable "create_action_build_failed"
    exit 0
fi

build_args=(--proposal-file "$action_file")
signing_args=()
txid="$(sdk_run_signal_safe "create_action_submit_signal" \
    build_sign_submit "info_${tok}" 0 "$deposit" build_args signing_args)"

if [ -z "$txid" ]; then
    gov_log "info action submit failed"
    sdk_sometimes false "info_action_created"
    exit 0
fi

# Publish the action ONLY after it is visible in gov-state, capturing its
# real index. The vote driver is an independent parallel command that
# finds work solely through these files, so a file must always point at a
# votable on-chain action.
ix=""
for _ in $(seq 1 30); do
    ix="$(gov_state | jq -r --arg t "$txid" \
        '.proposals[]? | select(.actionId.txId==$t) | .actionId.govActionIx' 2>/dev/null | head -1)"
    [ -n "$ix" ] && break
    sleep 2
done

if [ -z "$ix" ]; then
    gov_log "action $txid not visible in gov-state; not publishing"
    sdk_sometimes false "info_action_created"
    exit 0
fi

record_created "$txid" "$ix"
gov_log "info action created: ${txid}#${ix}"
sdk_sometimes true "info_action_created"

# Perturbation coverage: this action was created while the chain was
# recently stalled by faults.
if recent_stall 90; then
    sdk_sometimes true "gov_op_under_perturbation" \
        "$(jq -nc --arg op "create" '{op:$op}')"
fi
exit 0
