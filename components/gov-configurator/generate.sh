#!/usr/bin/env bash
# generate.sh — cardonnay-driven genesis + governance-asset generation
# for the cardano_node_governance Antithesis testnet.
#
# Strategy: cardonnay's `conway_fast` welds genesis/asset generation to
# node startup inside common-start-fast's main(). We materialize its
# scripts with `cardonnay create --generate-only` (template substitution
# only, no nodes), then run a generation-only main() — the prefix of the
# real main up to create_dreps_files, dropping every node-start /
# on-chain step. Those on-chain steps (register_entities_in_conway) are
# performed later by the gov-cli composer drivers against relay1.
#
# Output volumes (see docker-compose.yaml):
#   /configs/<i>        per-pool node config volume  (p<i>-configs)
#   /gov-data           gov-cli assets (governance_data, faucet, pools)
#   /gov-state          shelley genesis for cardano-clusterlib
#
# NOTE: end-to-end validation requires a real cardano-cli + cardonnay
# toolchain and a node boot; this environment can only check syntax.

set -Eeuo pipefail
trap 'echo "generate.sh failed at line $LINENO" >&2' ERR

NUM_POOLS="${NUM_POOLS:-3}"
# securityParam (k) and epochLength (slots). cardonnay's conway_fast
# defaults to k=10 / epochLength=1000 (k = 10·k/f with f=0.1), which is
# far too small for an Antithesis testnet: under network-partition faults
# a producer minority builds >k blocks during a multi-minute split, so
# the chain reorgs deeper than k. Raise k (3 producers give a 2-vs-1
# majority so only the minority reorgs; k must cover its block production
# over the longest split) and keep cardonnay's epochLength = 10·k/f
# ratio for nonce stability.
SECURITY_PARAM="${SECURITY_PARAM:-50}"
EPOCH_LENGTH="${EPOCH_LENGTH:-5000}"
GEN_ROOT=/work/cdny

# Generate once per volume lifetime. Every `docker compose up` starts
# this one-shot service again; without this guard it would regenerate a
# fresh genesis on each `up` and desync from already-running nodes.
# `docker compose down -v` wipes the volume and the marker, forcing a
# fresh genesis for the next run.
MARKER=/gov-data/.generated
if [ -f "$MARKER" ]; then
    echo "gov-configurator: assets already generated; skipping"
    exit 0
fi
# cardonnay derives STATE_CLUSTER as the directory containing the
# socket, so the socket must sit directly in state-cluster0 (not a
# sockets/ subdir) for STATE below to match.
SOCK="${GEN_ROOT}/state-cluster0/node.socket"
export CARDANO_NODE_SOCKET_PATH="$SOCK"

# Clean slate so a container restart re-generates idempotently.
rm -rf "$GEN_ROOT"
mkdir -p "$GEN_ROOT" "$(dirname "$SOCK")"
cd /tmp   # cardonnay refuses to run from inside the state dir

# ---------------------------------------------------------------------
# 1. Materialize the conway_fast scripts (generate-only = no nodes).
#    -s minimum is 3; we override NUM_POOLS in the materialized script.
#
#    cardonnay's CLI calls os.getlogin() to build its default workdir,
#    which throws in a container with no controlling terminal. Invoke
#    it through a shim that patches getlogin; everything else uses the
#    stable public CLI.
# ---------------------------------------------------------------------
export GEN_ROOT
GEN_ROOT="$GEN_ROOT" python3 - <<'PY'
import os, sys
os.getlogin = lambda: os.environ.get("USER") or "root"
from cardonnay.main import main
sys.argv = ["cardonnay", "create", "-t", "conway_fast",
            "-g", "-i", "0", "-s", "3", "--work-dir", os.environ["GEN_ROOT"]]
try:
    main()
except SystemExit as exc:
    sys.exit(exc.code or 0)
PY
SCRIPT_DIR="${GEN_ROOT}/cluster0_conway_fast"
START="${SCRIPT_DIR}/common-start-fast"
[ -f "$START" ] || { echo "expected $START from cardonnay" >&2; exit 1; }

# Raise securityParam (k) + epochLength in the shelley genesis spec that
# create_genesis reads (also feeds the byron --k). See the SECURITY_PARAM
# note above. Keep epochLength = 10·k/f so the nonce-stability window
# (3k/f) fits inside an epoch.
SPEC="${SCRIPT_DIR}/genesis.spec.json"
if [ -f "$SPEC" ]; then
    jq --argjson k "$SECURITY_PARAM" --argjson el "$EPOCH_LENGTH" \
        '.securityParam = $k | .epochLength = $el' "$SPEC" > "${SPEC}.tmp" \
        && mv "${SPEC}.tmp" "$SPEC"
    echo "genesis spec: securityParam=${SECURITY_PARAM} epochLength=${EPOCH_LENGTH}"
fi

# ---------------------------------------------------------------------
# 2. Patch the materialized start script:
#    a) force NUM_POOLS,
#    b) drop the ">= 3 pools" guard,
#    c) replace main() with a generation-only sequence.
# ---------------------------------------------------------------------
sed -i "s/^readonly NUM_POOLS=.*/readonly NUM_POOLS=${NUM_POOLS}/" "$START"

# Drop the guard block: `if [ "$NUM_POOLS" -lt 3 ]; then ... fi`
sed -i '/if \[ "\$NUM_POOLS" -lt 3 \]; then/,/^  fi$/d' "$START"

# Replace the body of main() with the generation-only prefix (mirrors
# the real main up to create_dreps_files; drops bft, supervisor, the
# cluster scripts, node start, on-chain registration and tx-generator).
awk '
/^main\(\) \{/ {
    print "main() {"
    print "  initialize_globals"
    print "  setup_state_cluster \"${STATE_CLUSTER}/create_staked\""
    print "  create_genesis"
    print "  create_committee_keys_in_genesis"
    print "  create_genesis_utxos"
    print "  get_genesis_data"
    print "  edit_node_configs"
    print "  create_pools_files"
    print "  create_dreps_files"
    print "  : > \"$START_CLUSTER_STATUS\""
    print "  echo \"Genesis + governance assets generated (generation-only)\""
    print "}"
    skip = 1
    next
}
skip && /^\}/ { skip = 0; next }
skip { next }
{ print }
' "$START" > "${START}.gen"
mv "${START}.gen" "$START"
chmod 0755 "$START"

# ---------------------------------------------------------------------
# 3. Run generation. Produces genesis + governance_data under STATE.
# ---------------------------------------------------------------------
STATE="${GEN_ROOT}/state-cluster0"
"$START"

[ -d "${STATE}/governance_data" ] || { echo "governance_data not generated" >&2; exit 1; }
[ -f "${STATE}/shelley/genesis.json" ] || { echo "shelley genesis missing" >&2; exit 1; }

# ---------------------------------------------------------------------
# 4. Distribute gov-cli assets.
# ---------------------------------------------------------------------
mkdir -p /gov-data/governance_data /gov-data/faucet /gov-data/pools /gov-data/state
cp -r "${STATE}/governance_data/." /gov-data/governance_data/
cp "${STATE}/shelley/genesis-utxo."* /gov-data/faucet/
for ((i = 1; i <= NUM_POOLS; i++)); do
    mkdir -p "/gov-data/pools/node-pool${i}"
    cp "${STATE}/nodes/node-pool${i}/cold.vkey" "/gov-data/pools/node-pool${i}/"
    cp "${STATE}/nodes/node-pool${i}/cold.skey" "/gov-data/pools/node-pool${i}/"
done

# clusterlib (python drivers) reads network params from shelley genesis.
mkdir -p /gov-state/shelley
cp "${STATE}/shelley/"genesis*.json /gov-state/shelley/

# ---------------------------------------------------------------------
# 5. Per-node config volumes for p1..pN (and relay reuses p1's).
#    Genesis files are copied next to the node config; genesis hashes
#    are deleted so the node recomputes them (matches the master
#    configurator). Topology + tracer wiring target the docker network.
# ---------------------------------------------------------------------
make_topology() {
    # $1 = this pool index; peers = the other pools + relay1
    local self="$1" peers=() j
    for ((j = 1; j <= NUM_POOLS; j++)); do
        [ "$j" -eq "$self" ] && continue
        peers+=("{\"address\":\"p${j}.example\",\"port\":3001}")
    done
    peers+=('{"address":"relay1.example","port":3001}')
    local joined; joined="$(IFS=,; echo "${peers[*]}")"
    cat <<EOF
{
  "localRoots": [
    { "accessPoints": [${joined}], "advertise": true, "trustable": true, "valency": ${#peers[@]} }
  ],
  "publicRoots": [],
  "useLedgerAfterSlot": 0
}
EOF
}

for ((i = 1; i <= NUM_POOLS; i++)); do
    POOL="/configs/${i}"
    mkdir -p "${POOL}/configs" "${POOL}/keys"

    cp "${STATE}/shelley/genesis.json"        "${POOL}/configs/shelley-genesis.json"
    cp "${STATE}/shelley/genesis.alonzo.json" "${POOL}/configs/alonzo-genesis.json"
    cp "${STATE}/shelley/genesis.conway.json" "${POOL}/configs/conway-genesis.json"
    cp "${STATE}/byron/genesis.json"          "${POOL}/configs/byron-genesis.json"

    # Node config: repoint genesis files to the in-container names, drop
    # genesis hashes (recomputed at boot), enable the trace forwarder so
    # --tracer-socket-path-connect works against the cardano-tracer.
    # cardonnay's config leaves TraceOptions empty, so the new
    # UseTraceDispatcher tracer emits everything at Info severity — a
    # firehose (~43M events) that Antithesis cannot materialize (trips
    # the "very high output" property and starves the sidecar's
    # log-presence assertions). Apply the master testnet's per-namespace
    # severity discipline (Silence/throttle the high-frequency
    # namespaces, keep forge/fork/peer events visible), plus silence
    # ChainDB.ImmDbEvent chunk-validation replay spam.
    jq '
        .ByronGenesisFile   = "byron-genesis.json"
      | .ShelleyGenesisFile = "shelley-genesis.json"
      | .AlonzoGenesisFile  = "alonzo-genesis.json"
      | .ConwayGenesisFile  = "conway-genesis.json"
      | del(.ByronGenesisHash, .ShelleyGenesisHash, .AlonzoGenesisHash, .ConwayGenesisHash)
      | .UseTraceDispatcher = true
      | .TurnOnLogging = true
      | .TraceOptions = {
          "": {"backends": ["EKGBackend", "Forwarder"], "detail": "DNormal", "severity": "Notice"},
          "BlockFetch.Client.CompletedBlockFetch": {"maxFrequency": 2},
          "BlockFetch.Decision": {"severity": "Silence"},
          "ChainDB": {"severity": "Info"},
          "ChainDB.AddBlockEvent.AddBlockValidation": {"severity": "Silence"},
          "ChainDB.AddBlockEvent.AddBlockValidation.ValidCandidate": {"maxFrequency": 2},
          "ChainDB.AddBlockEvent.AddedBlockToQueue": {"maxFrequency": 2},
          "ChainDB.AddBlockEvent.AddedBlockToVolatileDB": {"maxFrequency": 2},
          "ChainDB.CopyToImmutableDBEvent.CopiedBlockToImmutableDB": {"maxFrequency": 2},
          "ChainDB.ImmDbEvent": {"severity": "Silence"},
          "ChainSync.Client": {"severity": "Warning"},
          "Forge.Loop": {"severity": "Info"},
          "Forge.StateInfo": {"severity": "Info"},
          "Mempool": {"severity": "Silence"},
          "Net.ConnectionManager.Remote": {"severity": "Info"},
          "Net.ConnectionManager.Remote.ConnectionManagerCounters": {"severity": "Silence"},
          "Net.ErrorPolicy": {"severity": "Info"},
          "Net.ErrorPolicy.Local": {"severity": "Info"},
          "Net.InboundGovernor": {"severity": "Warning"},
          "Net.InboundGovernor.Remote": {"severity": "Info"},
          "Net.Mux.Remote": {"severity": "Info"},
          "Net.PeerSelection": {"severity": "Silence"},
          "Net.PeerSelection.Actions": {"severity": "Info"},
          "Net.Subscription.DNS": {"severity": "Info"},
          "Net.Subscription.IP": {"severity": "Info"},
          "Resources": {"severity": "Silence"},
          "Startup.DiffusionInit": {"severity": "Info"}
        }
    ' "${STATE}/config-pool${i}.json" > "${POOL}/configs/config.json"

    make_topology "$i" > "${POOL}/configs/topology.json"

    # Block-producer credentials referenced by the node command.
    cp "${STATE}/nodes/node-pool${i}/op.cert"  "${POOL}/keys/opcert.cert"
    cp "${STATE}/nodes/node-pool${i}/kes.skey" "${POOL}/keys/kes.skey"
    cp "${STATE}/nodes/node-pool${i}/vrf.skey" "${POOL}/keys/vrf.skey"
done

touch "$MARKER"
echo "gov-configurator: genesis + governance assets distributed"
