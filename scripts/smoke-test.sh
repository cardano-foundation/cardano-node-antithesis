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
    # Use `docker compose ps` to resolve the service to its actual container
    # name — testnets that allow multi-project coexistence on a shared host
    # don't pin `container_name:`, so the container is project-prefixed.
    STATE="$(docker compose -f "$COMPOSE_FILE" ps --format '{{.State}}' "$NODE" 2>/dev/null | head -1)"
    if [ "$STATE" = "exited" ] || [ "$STATE" = "dead" ]; then
      echo "FAIL: ${NODE} crashed"
      docker compose -f "$COMPOSE_FILE" logs --tail 30 "$NODE" 2>&1
      exit 1
    fi

    # Query the node tip via cardano-cli ping. `docker compose exec`
    # resolves the service name regardless of container_name.
    if TIP=$(docker compose -f "$COMPOSE_FILE" exec -T "$NODE" \
        cardano-cli ping --magic "$MAGIC" --host 127.0.0.1 --port 3001 \
        --tip --quiet -c1 2>/dev/null); then
      echo "OK: ${NODE} — ${TIP}"
      break
    fi

    sleep 5
  done
done

echo "Checking sidecar convergence command..."
docker compose -f "$COMPOSE_FILE" exec -T sidecar \
  /opt/antithesis/test/v1/convergence/eventually_converged.sh

# Adversary probe: if the testnet has the adversary container, exec
# its driver once to prove the binary runs, the chainpoints file is
# reachable, and exit code is zero. Antithesis composer dispatches
# this same script at much higher rate during a real run.
if docker compose -f "$COMPOSE_FILE" config --services | grep -qx adversary; then
  echo "Probing adversary driver..."
  if ! docker compose -f "$COMPOSE_FILE" exec -T adversary \
       /opt/antithesis/test/v1/chain-sync-client/parallel_driver_flap.sh; then
    echo "FAIL: adversary parallel_driver_flap.sh returned non-zero"
    docker compose -f "$COMPOSE_FILE" logs --tail 30 adversary 2>&1
    exit 1
  fi
  echo "OK: adversary driver"
fi

# tx-generator probes are conditional on the service being present in
# the resolved compose. The service is parked in some testnets (see
# tx-generator.disabled.yaml) — when it isn't part of the running
# cluster we skip the probes rather than fail the smoke test.
if ! docker compose -f "$COMPOSE_FILE" config --services | grep -qx tx-generator; then
  echo "SKIP: tx-generator not in compose for testnet ${TESTNET}, skipping tx-generator probes"
  echo "PASS: all ${POOLS} nodes responding"
  exit 0
fi

# tx-generator: prove the daemon comes up, drives one
# refill, lets the indexer observe it, then lands a small
# burst of transacts and grows the population. This is the
# CI-side counterpart of the docker-compose contract; the
# Antithesis composer drives the same scripts at much
# heavier load on the cluster.
TXGEN_SOCK=/state/tx-generator-control.sock
txgen_send() {
  docker exec tx-generator sh -c \
    "echo '$1' | nc -U -q 1 $TXGEN_SOCK"
}

echo "Waiting for tx-generator control socket..."
TXGEN_DEADLINE=$((SECONDS + 90))
while ! docker exec tx-generator test -S "$TXGEN_SOCK" 2>/dev/null; do
  if [ "$SECONDS" -ge "$TXGEN_DEADLINE" ]; then
    echo "FAIL: tx-generator control socket never bound"
    docker compose -f "$COMPOSE_FILE" logs --tail 50 tx-generator 2>&1
    exit 1
  fi
  sleep 2
done

echo "Probing tx-generator ready..."
TXGEN_DEADLINE=$((SECONDS + 180))
while true; do
  if [ "$SECONDS" -ge "$TXGEN_DEADLINE" ]; then
    echo "FAIL: tx-generator never reported ready=true"
    docker compose -f "$COMPOSE_FILE" logs --tail 50 tx-generator 2>&1
    exit 1
  fi
  RSP="$(txgen_send '{"ready":null}' 2>/dev/null || true)"
  if [ -n "$RSP" ] && echo "$RSP" | jq -e '.ready == true' >/dev/null 2>&1; then
    echo "OK: tx-generator ready"
    break
  fi
  sleep 3
done

echo "Driving initial refill..."
if ! docker exec tx-generator parallel_driver_refill; then
  echo "FAIL: parallel_driver_refill returned non-zero"
  docker compose -f "$COMPOSE_FILE" logs --tail 50 tx-generator 2>&1
  exit 1
fi

echo "Waiting for indexer to observe refill UTxO (p50_lovelace != null)..."
# populationSize is the count of HD-derived addresses
# (incremented by refill on disk before the UTxO lands).
# p50_lovelace is null until the indexer actually sees a
# UTxO on chain, so it's the right gate for "ready to
# transact".
TXGEN_DEADLINE=$((SECONDS + 180))
while true; do
  if [ "$SECONDS" -ge "$TXGEN_DEADLINE" ]; then
    echo "FAIL: indexer never observed refill UTxO"
    txgen_send '{"snapshot":null}' || true
    exit 1
  fi
  SNAP="$(txgen_send '{"snapshot":null}' 2>/dev/null || true)"
  P50="$(echo "$SNAP" | jq -r '.p50_lovelace' 2>/dev/null || echo null)"
  if [ "$P50" != "null" ] && [ -n "$P50" ]; then
    echo "OK: indexer observed UTxO (p50_lovelace=$P50)"
    break
  fi
  sleep 3
done

echo "Driving 5 transacts..."
TXGEN_OK=0
TXGEN_TMP="$(mktemp -d)"
trap 'rm -rf "$TXGEN_TMP"; cleanup' EXIT
for i in 1 2 3 4 5; do
  STDERR="$TXGEN_TMP/transact-$i.err"
  if docker exec tx-generator parallel_driver_transact 2>"$STDERR"; then
    TXGEN_OK=$((TXGEN_OK + 1))
  fi
  # The composer-side semantics treat exit 1 as
  # "not applicable, retry next tick", so we don't fail
  # the smoke-test on a non-zero exit. But a `jq:` line
  # on stderr means the driver itself crashed (malformed
  # JSON, missing field, etc.) — that's a real bug and
  # the smoke-test must surface it.
  if grep -q '^jq:' "$STDERR"; then
    echo "FAIL: parallel_driver_transact emitted jq error on transact #$i:"
    cat "$STDERR"
    exit 1
  fi
  sleep 2
done

echo "Final tx-generator snapshot:"
SNAP="$(txgen_send '{"snapshot":null}')"
echo "$SNAP" | jq .

POP="$(echo "$SNAP" | jq -r '.populationSize // 0')"
if [ "$TXGEN_OK" -lt 1 ]; then
  echo "FAIL: no transacts landed"
  exit 1
fi
if [ "$POP" -lt 2 ]; then
  echo "FAIL: populationSize did not grow past refill ($POP < 2)"
  exit 1
fi

echo "OK: tx-generator drove $TXGEN_OK/5 transacts, populationSize=$POP"

echo "PASS: all ${POOLS} nodes responding"
