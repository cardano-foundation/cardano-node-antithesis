#!/usr/bin/env bash
# helper_gov.sh — shared cardano-cli governance helpers.
#
# `helper_`-prefixed: ignored by the Antithesis composer scheduler.
# Sibling driver scripts source it. No `set -e` — callers decide how
# to react to a non-zero cardano-cli (faults can interrupt any call).
#
# Assets are produced by the gov-configurator (which runs cardonnay's
# conway_fast genesis + governance-asset generation) and mounted at
# $GOV. The on-chain governance operations below mirror cardonnay's
# `register_entities_in_conway` plus the create-info / vote flow from
# cardano-node-tests' governance_setup.py, executed against relay1.

: "${CARDANO_NODE_SOCKET_PATH:=/state/node.socket}"
: "${TESTNET_MAGIC:=42}"
: "${GOV:=/gov-data}"
: "${WORK:=/work}"
: "${NUM_DREPS:=5}"
: "${NUM_CC:=5}"
: "${NUM_POOLS:=2}"

export CARDANO_NODE_SOCKET_PATH

GD="$GOV/governance_data"               # cardonnay governance_data/
FAUCET_SKEY="$GOV/faucet/genesis-utxo.skey"
FAUCET_VKEY="$GOV/faucet/genesis-utxo.vkey"
STATE_DIR="$GOV/state"                  # writable coordination area
ACTIONS_DIR="$STATE_DIR/actions"        # one file per created action
SETUP_MARKER="$STATE_DIR/setup_done"
MAGIC=(--testnet-magic "$TESTNET_MAGIC")
ANCHOR_URL="https://example.com/governance.json"

gov_log() { echo "[gov $(basename "${0:-helper}")] $*" >&2; }

cli() { cardano-cli "$@"; }

faucet_addr() { cat "$GOV/faucet/genesis-utxo.addr"; }

# wait_for_node [max_tries] — block until the local node answers a tip
# query and has advanced past slot 0. Returns 0 ready, 1 timed out.
wait_for_node() {
    local tries="${1:-150}" slot i
    for ((i = 0; i < tries; i++)); do
        slot="$(cli query tip "${MAGIC[@]}" 2>/dev/null | jq -r '.slot // 0' 2>/dev/null)"
        if [ -n "$slot" ] && [ "$slot" -gt 0 ] 2>/dev/null; then
            return 0
        fi
        sleep 2
    done
    return 1
}

current_epoch() { cli query tip "${MAGIC[@]}" 2>/dev/null | jq -r '.epoch // empty'; }

# wait_for_epoch <target> [max_seconds] — poll until the chain reaches
# the target epoch. Bounded so faults can't hang us forever.
wait_for_epoch() {
    local target="$1" max="${2:-600}" waited=0 ep
    while ((waited < max)); do
        ep="$(current_epoch)"
        if [ -n "$ep" ] && [ "$ep" -ge "$target" ] 2>/dev/null; then
            return 0
        fi
        sleep 5
        waited=$((waited + 5))
    done
    return 1
}

gov_state() { cli conway query gov-state "${MAGIC[@]}" 2>/dev/null; }

gov_action_deposit() {
    gov_state \
        | jq -r '.currentPParams.govActionDeposit
                 // .nextRatifyState.nextEnactState.curPParams.govActionDeposit
                 // 100000000'
}

# faucet_txin — echo the largest faucet UTxO as a TxIn string. The
# genesis-utxo address holds one huge UTxO, so a single input always
# covers fees + deposits for our small txs.
faucet_txin() {
    local addr; addr="$(faucet_addr)"
    cli query utxo --address "$addr" "${MAGIC[@]}" --output-json 2>/dev/null \
        | jq -r 'to_entries | max_by(.value.value.lovelace) | .key'
}

# All drivers fund fees + deposits from the single genesis-utxo faucet,
# whose change output is itself a single UTxO. Concurrent spenders would
# pick the same input and conflict, so faucet spends are serialized with
# a mkdir lock held across build -> submit -> spend-confirmation.
FAUCET_LOCK="$STATE_DIR/faucet.lock"

faucet_lock() {
    local tries="${1:-120}" i
    for ((i = 0; i < tries; i++)); do
        if mkdir "$FAUCET_LOCK" 2>/dev/null; then
            return 0
        fi
        sleep 1
    done
    return 1
}
faucet_unlock() { rmdir "$FAUCET_LOCK" 2>/dev/null || true; }

# _wait_spent <txin> [max_seconds] — block until the input is gone from
# the faucet UTxO set, i.e. the tx is on-chain and the change is settled.
_wait_spent() {
    local txin="$1" max="${2:-60}" addr waited=0
    addr="$(faucet_addr)"
    while ((waited < max)); do
        if ! cli query utxo --address "$addr" "${MAGIC[@]}" --output-json 2>/dev/null \
            | jq -e --arg k "$txin" 'has($k)' >/dev/null 2>&1; then
            return 0
        fi
        sleep 2
        waited=$((waited + 2))
    done
    return 1
}

# build_sign_submit <out_basename> <witness_count> <build_args_arr> <signing_args_arr>
# Builds against the faucet (change back to faucet), signs with the
# supplied signing-key args (faucet key always added), submits, and
# echoes the txid. Serialized on the faucet lock. Returns non-zero on
# any cardano-cli failure.
build_sign_submit() {
    local base="$1" witnesses="$2"
    local -n _bargs="$3"
    local -n _sargs="$4"
    local addr txin body tx txid rc=0
    addr="$(faucet_addr)"
    body="$WORK/${base}.txbody"
    tx="$WORK/${base}.tx"

    faucet_lock || { gov_log "faucet lock timeout"; return 1; }

    txin="$(faucet_txin)"
    if [ -z "$txin" ]; then
        gov_log "no faucet UTxO"
        faucet_unlock
        return 1
    fi

    if ! cli conway transaction build \
        --tx-in "$txin" \
        --change-address "$addr" \
        "${_bargs[@]}" \
        --witness-override "$witnesses" \
        "${MAGIC[@]}" \
        --out-file "$body"; then
        faucet_unlock
        return 1
    fi

    if ! cli conway transaction sign \
        "${_sargs[@]}" \
        --signing-key-file "$FAUCET_SKEY" \
        "${MAGIC[@]}" \
        --tx-body-file "$body" \
        --out-file "$tx"; then
        faucet_unlock
        return 1
    fi

    if ! cli conway transaction submit --tx-file "$tx" "${MAGIC[@]}"; then
        faucet_unlock
        return 1
    fi

    _wait_spent "$txin" || gov_log "spend confirm timed out for $txin"
    faucet_unlock

    txid="$(cli conway transaction txid --tx-file "$tx" 2>/dev/null)"
    echo "$txid"
    return "$rc"
}

# anchor_hash — deterministic anchor data hash for info actions/votes.
anchor_hash() {
    local text='{"body":{"title":"antithesis governance workload"}}'
    cli conway governance hash anchor-data --text "$text" 2>/dev/null
}

# pending_action — atomically claim one un-voted action via mkdir lock.
# Echoes "txid ix file"; returns non-zero when none available. Lets
# concurrent vote-driver instances avoid double-claiming.
pending_action() {
    local f lock txid ix
    shopt -s nullglob
    for f in "$ACTIONS_DIR"/*.action; do
        lock="${f}.claim"
        if mkdir "$lock" 2>/dev/null; then
            txid="$(jq -r '.txid' "$f" 2>/dev/null)"
            ix="$(jq -r '.ix' "$f" 2>/dev/null)"
            echo "$txid $ix $f"
            return 0
        fi
    done
    return 1
}
