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

echo "Validating Antithesis-compatible image references..."
while IFS= read -r IMAGE; do
  if [ -z "$IMAGE" ]; then
    echo "FAIL: compose produced an empty image reference"
    exit 1
  fi
  if [[ "$IMAGE" =~ :[^/@]+@sha256: ]]; then
    echo "FAIL: Antithesis rejects tag-plus-digest image references: $IMAGE"
    echo "Use either repo:tag or repo@sha256:digest; prefer digest-only for release pins."
    exit 1
  fi
done < <(docker compose --progress quiet -f "$COMPOSE_FILE" config --images)

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

if [ "$TESTNET" = "cardano_amaru" ]; then
  BOOTSTRAP_TIMEOUT="${AMARU_BOOTSTRAP_SMOKE_TIMEOUT:-1200}"
  BOOTSTRAP_DEADLINE=$((SECONDS + BOOTSTRAP_TIMEOUT))

  echo "Waiting for bootstrap-state-snapshot to complete (timeout ${BOOTSTRAP_TIMEOUT}s)..."
  while true; do
    STATE="$(docker inspect -f '{{.State.Status}} {{.State.ExitCode}}' bootstrap-state-snapshot 2>/dev/null || true)"
    STATUS="${STATE%% *}"
    EXIT_CODE="${STATE#* }"
    if [ "$STATUS" = "exited" ]; then
      if [ "$EXIT_CODE" = "0" ]; then
        echo "OK: bootstrap-state-snapshot completed"
        break
      fi
      echo "FAIL: bootstrap-state-snapshot exited with code ${EXIT_CODE}"
      docker compose -f "$COMPOSE_FILE" logs --tail 80 bootstrap-state-snapshot 2>&1
      exit 1
    fi
    if [ "$STATUS" = "dead" ]; then
      echo "FAIL: bootstrap-state-snapshot is dead"
      docker compose -f "$COMPOSE_FILE" logs --tail 80 bootstrap-state-snapshot 2>&1
      exit 1
    fi
    if [ "$SECONDS" -ge "$BOOTSTRAP_DEADLINE" ]; then
      echo "FAIL: bootstrap-state-snapshot did not complete within ${BOOTSTRAP_TIMEOUT}s"
      docker compose -f "$COMPOSE_FILE" logs --tail 80 bootstrap-state-snapshot 2>&1
      exit 1
    fi
    sleep 5
  done

  echo "Waiting for bootstrap-producer to complete (timeout ${BOOTSTRAP_TIMEOUT}s)..."
  while true; do
    STATE="$(docker inspect -f '{{.State.Status}} {{.State.ExitCode}}' bootstrap-producer 2>/dev/null || true)"
    STATUS="${STATE%% *}"
    EXIT_CODE="${STATE#* }"
    if [ "$STATUS" = "exited" ]; then
      if [ "$EXIT_CODE" = "0" ]; then
        echo "OK: bootstrap-producer completed"
        break
      fi
      echo "FAIL: bootstrap-producer exited with code ${EXIT_CODE}"
      docker compose -f "$COMPOSE_FILE" logs --tail 80 bootstrap-producer 2>&1
      exit 1
    fi
    if [ "$STATUS" = "dead" ]; then
      echo "FAIL: bootstrap-producer is dead"
      docker compose -f "$COMPOSE_FILE" logs --tail 80 bootstrap-producer 2>&1
      exit 1
    fi
    if [ "$SECONDS" -ge "$BOOTSTRAP_DEADLINE" ]; then
      echo "FAIL: bootstrap-producer did not complete within ${BOOTSTRAP_TIMEOUT}s"
      docker compose -f "$COMPOSE_FILE" logs --tail 80 bootstrap-producer 2>&1
      exit 1
    fi
    sleep 5
  done

  for RELAY in amaru-relay-1 amaru-relay-2; do
    for ENV_REQUIREMENT in AMARU_LOG=warn AMARU_TRACE=warn AMARU_COLOR=never; do
      if ! docker inspect -f '{{range .Config.Env}}{{println .}}{{end}}' "$RELAY" \
          | grep -Fxq "$ENV_REQUIREMENT"; then
        echo "FAIL: ${RELAY} missing ${ENV_REQUIREMENT}"
        docker inspect -f '{{range .Config.Env}}{{println .}}{{end}}' "$RELAY" 2>&1
        exit 1
      fi
    done

    echo "Waiting for ${RELAY} to consume bootstrap bundle..."
    RELAY_DEADLINE=$((SECONDS + 180))
    while ! docker exec "$RELAY" sh -c \
        'test -d /srv/amaru/ledger.testnet_42.db/live \
          && test -d /srv/amaru/chain.testnet_42.db \
          && test -f /srv/amaru/nonces.json' 2>/dev/null; do
      STATE="$(docker inspect -f '{{.State.Status}}' "$RELAY" 2>/dev/null || true)"
      if [ "$STATE" = "exited" ] || [ "$STATE" = "dead" ]; then
        echo "FAIL: ${RELAY} stopped before consuming bootstrap bundle"
        docker compose -f "$COMPOSE_FILE" logs --tail 80 "$RELAY" 2>&1
        exit 1
      fi
      if [ "$SECONDS" -ge "$RELAY_DEADLINE" ]; then
        echo "FAIL: ${RELAY} did not copy bootstrap bundle"
        docker compose -f "$COMPOSE_FILE" logs --tail 80 "$RELAY" 2>&1
        exit 1
      fi
      sleep 5
    done

    RESTARTS_BEFORE="$(docker inspect -f '{{.RestartCount}}' "$RELAY")"
    sleep 20
    STATE="$(docker inspect -f '{{.State.Status}}' "$RELAY" 2>/dev/null || true)"
    RESTARTS_AFTER="$(docker inspect -f '{{.RestartCount}}' "$RELAY" 2>/dev/null || echo unknown)"
    if [ "$STATE" != "running" ] || [ "$RESTARTS_AFTER" != "$RESTARTS_BEFORE" ]; then
      echo "FAIL: ${RELAY} did not stay running after bootstrap bundle load"
      echo "state=${STATE} restarts_before=${RESTARTS_BEFORE} restarts_after=${RESTARTS_AFTER}"
      docker compose -f "$COMPOSE_FILE" logs --tail 80 "$RELAY" 2>&1
      exit 1
    fi
    echo "OK: ${RELAY} consumed bundle and stayed running"
  done
fi

echo "PASS: all ${POOLS} nodes responding"
