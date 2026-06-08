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
STATE_DIR="$GOV/state"                  # durable coordination area (gov-cli is fault-excluded)
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

# ensure_pparams — write current protocol parameters once (for fee calc
# and deposit amounts). Returns non-zero if the query fails.
ensure_pparams() {
    [ -s "$WORK/pparams.json" ] && return 0
    cli conway query protocol-parameters "${MAGIC[@]}" --out-file "$WORK/pparams.json" 2>/dev/null
    [ -s "$WORK/pparams.json" ]
}
pp_get() { jq -r "$1 // 0" "$WORK/pparams.json" 2>/dev/null; }
stake_deposit() { ensure_pparams && pp_get '.stakeAddressDeposit'; }
drep_deposit()  { ensure_pparams && pp_get '.dRepDeposit'; }
gov_deposit()   { ensure_pparams && pp_get '.govActionDeposit'; }

current_slot() { cli query tip "${MAGIC[@]}" 2>/dev/null | jq -r '.slot // 0'; }

# build_sign_submit <base> <txout_total> <deposit_total> <build_args_arr> <signing_args_arr>
# build-raw flow: explicit faucet input, explicit deposits, a calculated
# fee, and change back to the faucet. Unlike `transaction build`,
# build-raw performs NO anchor download/validation and never contacts
# the node, so proposals with unreachable anchor URLs work. `txout_total`
# is the lovelace in the caller's own --tx-out entries; `deposit_total`
# is the sum of cert/proposal deposits. Echoes the txid; serialized on
# the faucet lock.
build_sign_submit() {
    local base="$1" txout_total="$2" deposit_total="$3"
    local -n _bargs="$4"
    local -n _sargs="$5"
    local addr utxo txin in_amount ttl wcount fee change change0 draft body tx
    addr="$(faucet_addr)"
    draft="$WORK/${base}.draft"
    body="$WORK/${base}.txbody"
    tx="$WORK/${base}.tx"

    ensure_pparams || { gov_log "no pparams"; return 1; }
    faucet_lock || { gov_log "faucet lock timeout"; return 1; }

    # Parse the cardano-cli text table, NOT --output-json: jq (1.6) stores
    # numbers as doubles and silently corrupts lovelace amounts above 2^53
    # (the faucet holds ~1.5e16), which breaks value conservation by 1.
    # awk keeps the amount as the verbatim integer string ($3); the >m
    # float compare is only used to pick the largest UTxO.
    local sel
    sel="$(cli query utxo --address "$addr" "${MAGIC[@]}" --output-text 2>/dev/null \
        | awk 'NR>2 && $3+0>m {m=$3+0; tx=$1"#"$2; amt=$3} END{if (tx) print tx" "amt}')"
    txin="${sel% *}"
    in_amount="${sel##* }"
    if [ -z "$txin" ] || [ -z "$in_amount" ]; then
        gov_log "no faucet UTxO"; faucet_unlock; return 1
    fi

    ttl=$(( $(current_slot) + 2000 ))
    wcount=$(( ${#_sargs[@]} / 2 + 1 ))   # signing-key files + faucet key

    # Draft with fee 0 to size the tx, then calculate the real fee.
    change0=$(( in_amount - txout_total - deposit_total ))
    if ! cli conway transaction build-raw \
        --tx-in "$txin" \
        "${_bargs[@]}" \
        --tx-out "${addr}+${change0}" \
        --fee 0 \
        --invalid-hereafter "$ttl" \
        --out-file "$draft" >&2; then
        faucet_unlock; return 1
    fi

    fee="$(cli conway transaction calculate-min-fee \
        --tx-body-file "$draft" \
        --protocol-params-file "$WORK/pparams.json" \
        --witness-count "$wcount" 2>/dev/null | grep -oE '[0-9]+' | head -1)"
    [ -n "$fee" ] || { gov_log "fee calc failed"; faucet_unlock; return 1; }

    change=$(( in_amount - txout_total - deposit_total - fee ))
    if [ "$change" -lt 0 ]; then
        gov_log "insufficient faucet funds"; faucet_unlock; return 1
    fi

    if ! cli conway transaction build-raw \
        --tx-in "$txin" \
        "${_bargs[@]}" \
        --tx-out "${addr}+${change}" \
        --fee "$fee" \
        --invalid-hereafter "$ttl" \
        --out-file "$body" >&2; then
        faucet_unlock; return 1
    fi

    if ! cli conway transaction sign \
        "${_sargs[@]}" \
        --signing-key-file "$FAUCET_SKEY" \
        "${MAGIC[@]}" \
        --tx-body-file "$body" \
        --out-file "$tx" >&2; then
        faucet_unlock; return 1
    fi

    if ! cli conway transaction submit --tx-file "$tx" "${MAGIC[@]}" >&2; then
        faucet_unlock; return 1
    fi

    _wait_spent "$txin" || gov_log "spend confirm timed out for $txin"
    faucet_unlock

    # `transaction txid` returns JSON {"txhash":"..."} in cli 11.0; older
    # versions print the bare hex. Normalise to the bare hex.
    local raw
    raw="$(cli conway transaction txid --tx-file "$tx" 2>/dev/null)"
    if [[ "$raw" == \{* ]]; then
        jq -r '.txhash' <<<"$raw"
    else
        echo "$raw"
    fi
}

# anchor_hash — deterministic anchor data hash for info actions/votes.
anchor_hash() {
    local text='{"body":{"title":"antithesis governance workload"}}'
    cli hash anchor-data --text "$text" 2>/dev/null
}

# Action selection: the chain IS the queue.
#
# Rather than maintaining a local created/rejected ledger (which a
# transient fault could corrupt into permanently dropping a still-live
# action), the vote driver reads the live set of governance actions
# straight from the node via an N2C gov-state query. relay1 is
# fault-excluded and a LocalStateQuery returns the last settled ledger
# state, so this answers correctly even while block production is
# stalled. An action leaves the set only when the LEDGER expires or
# enacts it — there is no local "retire", so a transient submit/lock
# failure is self-healing (the action reappears next tick).

# live_info_actions — emit the current InfoAction proposals as a JSON
# array (each element is a full gov-state proposal). Empty array if none
# or if the node can't be reached.
live_info_actions() {
    gov_state | jq -c \
        '[.proposals[]? | select(.proposalProcedure.govAction.tag == "InfoAction")]' \
        2>/dev/null || printf '[]'
}

# antithesis_rng — a random non-negative integer (decimal digits only),
# steered by the Antithesis hypervisor when `antithesis_random` is
# present, otherwise from /dev/urandom. This is how the test surface is
# handed to Antithesis: every random choice the vote driver makes (which
# action, which voter, yes/no) flows through here.
antithesis_rng() {
    local r
    r="$(timeout 2 antithesis_random 2>/dev/null | tr -cd '0-9')"
    [ -n "$r" ] || r="$(od -An -tu4 -N4 /dev/urandom | tr -cd '0-9')"
    printf '%s' "${r:-0}"
}

# rng_mod <n> — an index in [0, n) from antithesis_rng. The magnitude is
# bounded to the last 9 digits so the modulo stays inside bash 64-bit
# arithmetic regardless of how wide the RNG output is.
rng_mod() {
    local n="$1" r
    [ "$n" -gt 0 ] 2>/dev/null || { printf '0'; return; }
    r="$(antithesis_rng)"; r="${r: -9}"
    printf '%s' "$(( 10#${r:-0} % n ))"
}

# Perturbation-witness state. The anytime_chain_progress probe writes a
# verdict here ("stalled <epoch>" / "producing <epoch>") each time it
# samples block production; the create/vote drivers read it to assert
# that a governance op landed while the chain was degraded.
CHAIN_VERDICT="$STATE_DIR/chain_verdict"

set_chain_verdict() {   # set_chain_verdict <stalled|producing>
    mkdir -p "$STATE_DIR"
    printf '%s %s\n' "$1" "$(date +%s)" > "$CHAIN_VERDICT" 2>/dev/null || true
}

# recent_stall [within_seconds] — true if the most recent chain sample
# was a stall within the window (i.e. faults were actively halting block
# production around now).
recent_stall() {
    local within="${1:-90}" v ts now
    [ -f "$CHAIN_VERDICT" ] || return 1
    read -r v ts < "$CHAIN_VERDICT" 2>/dev/null || return 1
    [ "$v" = "stalled" ] || return 1
    now="$(date +%s)"
    [ $(( now - ${ts:-0} )) -le "$within" ] 2>/dev/null
}
