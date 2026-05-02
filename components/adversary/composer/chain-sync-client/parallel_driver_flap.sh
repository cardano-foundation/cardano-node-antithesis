#!/usr/bin/env bash
#
# parallel_driver_flap.sh — fire one initiator-only chain-sync
# session at a randomly-chosen target node.
#
# Antithesis composer dispatches this script. We delegate every
# decision (which node to attack, which chain point to start from)
# to the cardano-adversary binary, seeded from $(antithesis_random)
# so the hypervisor steers the run.
#
# Targets include both producers (p1/p2/p3) and relays
# (relay1/relay2). All five speak the same N2N chain-sync; an
# initiator-only misbehaving peer must not break the cluster
# regardless of which one it connects to.
#
# Lifecycle: exec the binary, replace the shell. If Antithesis
# kills the container mid-attack, the next tick spawns a fresh
# invocation. No state to leak.

set -euo pipefail

SEED=$(antithesis_random 2>/dev/null \
    || od -An -tx8 -N8 /dev/urandom | tr -d ' \n')

exec cardano-adversary \
    --network-magic 42 \
    --target-host p1.example \
    --target-host p2.example \
    --target-host p3.example \
    --target-host relay1.example \
    --target-host relay2.example \
    --target-port 3001 \
    --chain-points-file /tracer/chainPoints.log \
    --seed "0x${SEED}" \
    --limit 100
