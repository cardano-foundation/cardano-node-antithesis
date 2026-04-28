#!/usr/bin/env bash

# Smoke test: verify the testnet compose starts and nodes respond to queries.
# Uses cardano-cli ping to prove each node is serving the Ouroboros protocol.
#
# Usage: scripts/smoke-test.sh [testnet] [timeout_seconds]
set -euo pipefail

TESTNET="${1:-cardano_node_master}"
TIMEOUT="${2:-120}"
COMPOSE_FILE="testnets/${TESTNET}/docker-compose.yaml"
MAGIC=42

export INTERNAL_NETWORK=true

cleanup() {
  echo "Tearing down..."
  docker compose --progress quiet -f "$COMPOSE_FILE" down --volumes --remove-orphans 2>/dev/null || true
}
trap cleanup EXIT

echo "Starting testnet ${TESTNET}..."
docker compose --progress quiet -f "$COMPOSE_FILE" up -d

POOLS=$(awk '/poolCount: /{ print $2 }' "testnets/${TESTNET}/testnet.yaml")
DEADLINE=$((SECONDS + TIMEOUT))

echo "Waiting for ${POOLS} nodes to respond to cardano-cli ping (timeout ${TIMEOUT}s)..."

for i in $(seq 1 "$POOLS"); do
  NODE="p${i}"
  while true; do
    if [ "$SECONDS" -ge "$DEADLINE" ]; then
      echo "FAIL: timed out waiting for ${NODE}"
      docker compose -f "$COMPOSE_FILE" logs --tail 20 "$NODE" 2>&1
      exit 1
    fi

    # Check the node did not terminate. Compose may briefly report created or
    # restarting while dependencies settle, so only terminal states fail.
    STATE="$(docker inspect -f '{{.State.Status}}' "$NODE" 2>/dev/null || true)"
    if [ "$STATE" = "exited" ] || [ "$STATE" = "dead" ]; then
      echo "FAIL: ${NODE} crashed"
      docker compose -f "$COMPOSE_FILE" logs --tail 30 "$NODE" 2>&1
      exit 1
    fi

    # Query the node tip via cardano-cli ping
    if TIP=$(docker exec "$NODE" \
        cardano-cli ping --magic "$MAGIC" --host 127.0.0.1 --port 3001 \
        --tip --quiet -c1 2>/dev/null); then
      echo "OK: ${NODE} — ${TIP}"
      break
    fi

    sleep 5
  done
done

echo "Checking sidecar convergence command..."
docker exec sidecar \
  /opt/antithesis/test/v1/convergence/eventually_converged.sh

echo "PASS: all ${POOLS} nodes responding"
