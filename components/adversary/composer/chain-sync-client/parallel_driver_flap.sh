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

# Bound the antithesis_random call — empty additional_stderr/stdout
# on the rare failing invocation in run d4c2996 + 61ad5ef showed the
# binary never started, the script hung for 15s in $(antithesis_random)
# until composer's per-command hard timeout SIGKILL'd it (exit 255).
# Fall back to /dev/urandom whenever antithesis_random takes longer
# than 2s; the seed still drives reproducible target/point picks.
SEED=$(timeout 2 antithesis_random 2>/dev/null \
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
