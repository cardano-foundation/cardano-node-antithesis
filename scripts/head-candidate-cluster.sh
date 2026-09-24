#!/usr/bin/env bash

# Local cluster proof for a rendered HEAD-candidate model (#215 AL-2).
#
# Usage: scripts/head-candidate-cluster.sh <rendered-model> [timeout_seconds] [images-file]
#
# The testnet pins a container name for every service and a fixed network
# name, so two clusters of this family cannot share a daemon. This command
# therefore derives a THROWAWAY EXECUTION COPY of the rendered model —
# container_name entries and the fixed network name removed, its own compose
# project — and runs the cluster from the copy only. Service names,
# hostnames, images and the census are identical to the rendered model; the
# copy is never the receipt's rendered_model and is deleted with the cluster.
#
# It waits for every producer and relay to answer a cardano-cli ping, samples
# the chain tip twice to show blocks advancing, records one
# `node-image <service> <ref>` line per node container (docker inspect, while
# running), then tears the cluster down. Exits non-zero if any node fails to
# answer or the chain does not advance.
set -euo pipefail

model=${1:?rendered model path is required}
timeout=${2:-900}
images_file=${3:-}

[ -f "$model" ] || {
  printf 'head-candidate-cluster: rendered model is absent: %s\n' "$model" >&2
  exit 1
}
model_dir=$(cd "$(dirname "$model")" && pwd)
MAGIC=42
export INTERNAL_NETWORK=true

fail() {
  printf 'head-candidate-cluster: %s\n' "$*" >&2
  exit 1
}

scratch=$(mktemp -d /tmp/head-candidate-cluster.XXXXXX) ||
  fail 'could not create the local execution scratch directory'
exec_model=$scratch/docker-compose.yaml

# The execution copy differs from the rendered model only by the two
# coexistence edits: no pinned container names, no fixed network name.
sed -e '/^[[:space:]]*container_name:/d' \
  -e '/^[[:space:]]*name: cardano-node-testnet$/d' \
  "$model" >"$exec_model" ||
  fail 'could not derive the local execution copy'

# The model mounts its side files relative to its own directory, so the copy
# must carry them too or the configurator reads empty mounts.
find "$model_dir" -maxdepth 1 -type f ! -name docker-compose.yaml \
  -exec cp -t "$scratch" {} + ||
  fail 'could not copy the model side files into the execution copy'

cleanup() {
  docker compose --progress quiet -f "$exec_model" down --volumes \
    --remove-orphans || true
  rm -rf -- "$scratch"
}
trap cleanup EXIT
trap 'cleanup; exit 143' TERM INT

compose() {
  docker compose --progress quiet -f "$exec_model" "$@"
}

printf 'local-execution-copy %s (container names and fixed network name removed; not the receipt rendered_model %s)\n' \
  "$exec_model" "$model"

resolved_config=$(compose config)

compose_service_image() {
  local service=$1
  awk -v svc="$service" '
    /^  [A-Za-z0-9_.-]+:$/ {
      service = $0
      sub(/:$/, "", service)
      gsub(/^ +/, "", service)
      next
    }
    service == svc && /^    image: / {
      image = $0
      sub(/^ *image: */, "", image)
      gsub(/^"|"$/, "", image)
      print image
      exit
    }
  ' <<<"$resolved_config"
}

# The census is discovered from the resolved model, never hardcoded: every
# service matching a producer or relay name must answer.
mapfile -t node_services < <(awk '
  /^  [A-Za-z0-9_.-]+:$/ {
    service = $0
    sub(/:$/, "", service)
    gsub(/^ +/, "", service)
    if (service ~ /^(p[0-9]+|relay[0-9]+)$/) print service
    next
  }
' <<<"$resolved_config")
[ "${#node_services[@]}" -ge 3 ] ||
  fail "resolved model carries fewer than three node services: ${#node_services[@]}"

# Every node service must resolve to one identical image reference.
node_images=$(printf '%s\n' "${node_services[@]}" |
  while IFS= read -r service; do compose_service_image "$service"; done |
  sort -u)
[ "$(wc -l <<<"$node_images")" -eq 1 ] ||
  fail "node services do not resolve to one image:
$node_images"
candidate_ref=$node_images
case "$candidate_ref" in
  *:????????????????????????????????????????@sha256:*) ;;
  *) fail "candidate image is not a tagged digest reference: $candidate_ref" ;;
esac

pools=$(awk '/poolCount: /{ print $2 }' "$model_dir/testnet.yaml")
[ "${pools:-0}" -ge 1 ] || fail "poolCount is unreadable from $model_dir/testnet.yaml"
producers=$(printf '%s\n' "${node_services[@]}" | grep -cE '^p[0-9]+$')
[ "$producers" -eq "$pools" ] ||
  fail "producer census $producers differs from poolCount $pools"

printf 'Starting rendered model %s (candidate %s)...\n' "$exec_model" "$candidate_ref"
compose up -d

deadline=$((SECONDS + timeout))
wait_for_ping() {
  local service=$1
  while true; do
    if [ "$SECONDS" -ge "$deadline" ]; then
      printf 'FAIL: timed out waiting for %s\n' "$service" >&2
      compose logs --tail 20 "$service" >&2 || true
      exit 1
    fi
    state=$(compose ps --format '{{.State}}' "$service" 2>/dev/null | head -1)
    if [ "$state" = exited ] || [ "$state" = dead ]; then
      printf 'FAIL: %s crashed\n' "$service" >&2
      compose logs --tail 30 "$service" >&2 || true
      exit 1
    fi
    # The candidate image's cardano-cli speaks the current ping surface:
    # a positional address, -m for the network magic, --mode tip.
    if tip=$(compose exec -T "$service" \
      cardano-cli ping 127.0.0.1:3001 -m "$MAGIC" --mode tip --quiet \
      -c1 2>/dev/null); then
      printf 'OK: %s — %s\n' "$service" "$tip"
      return 0
    fi
    sleep 5
  done
}

for service in "${node_services[@]}"; do
  wait_for_ping "$service"
done

tip_block() {
  compose exec -T relay1 cardano-cli query tip \
    --testnet-magic "$MAGIC" --socket-path /state/node.socket 2>/dev/null |
    sed -nE 's/.*"block"[[:space:]]*:[[:space:]]*([0-9]+).*/\1/p' | head -1
}

printf 'Sampling chain tip on relay1...\n'
first_tip=$(tip_block)
[ -n "$first_tip" ] || fail 'could not read a chain tip from relay1'
sleep 30
second_tip=$(tip_block)
[ -n "$second_tip" ] || fail 'could not read a second chain tip from relay1'
printf 'chain-tip first-block=%s second-block=%s\n' "$first_tip" "$second_tip"
[ "$second_tip" -gt "$first_tip" ] ||
  fail "chain did not advance between samples ($first_tip -> $second_tip)"

# One image line per node container, taken with docker inspect while the
# cluster runs. The container's resolved image ID must equal the candidate
# image's local ID; the printed reference is the candidate's immutable ref.
candidate_image_id=$(docker image inspect \
  --format '{{.Id}}' "${candidate_ref%@*}")
record_node_image() {
  local service=$1
  local container_id container_image_id
  container_id=$(compose ps -q "$service")
  [ -n "$container_id" ] || fail "no container for service $service"
  container_image_id=$(docker inspect --format '{{.Image}}' "$container_id")
  [ "$container_image_id" = "$candidate_image_id" ] ||
    fail "$service runs $container_image_id, not the candidate $candidate_image_id"
  printf 'node-image %s %s\n' "$service" "$candidate_ref"
}

for service in "${node_services[@]}"; do
  record_node_image "$service"
done | tee "${images_file:-/dev/null}"

printf 'PASS: %d node services answering on the candidate image; chain advancing\n' \
  "${#node_services[@]}"
