#!/usr/bin/env bash
# Mechanical gate for the feat/amaru-consumer PR (#179). Fast checks only —
# the slow cardano_amaru bring-up smoke is the slice PROOF, run separately by
# the driver and recorded in WIP.md, not on every commit.
set -euo pipefail

# No leftover conflict/whitespace markers.
git diff --check

# Compose model still validates with the consumer added.
INTERNAL_NETWORK=false docker compose \
  -f testnets/cardano_amaru/docker-compose.yaml config >/dev/null

# Consumer topology (once it exists) is valid JSON, amaru-relays-only.
topo=testnets/cardano_amaru/amaru-consumer-topology.json
if [ -f "$topo" ]; then
  jq -e . "$topo" >/dev/null
  # localRoots must reference only the amaru relays — no producer/relay peers.
  if jq -e '[.localRoots[].accessPoints[].address]
            | any(. == "p1.example" or . == "p2.example" or . == "p3.example"
                  or . == "relay1.example" or . == "relay2.example")' \
       "$topo" >/dev/null; then
    echo "gate: amaru-consumer topology references a non-amaru peer" >&2
    exit 1
  fi
fi

# Smoke script stays shell-valid.
bash -n scripts/smoke-test.sh

echo "gate: OK"
