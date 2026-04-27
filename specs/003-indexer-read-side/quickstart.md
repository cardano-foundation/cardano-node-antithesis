# Quickstart: Indexer-Backed Read Side for Test Workloads

**Feature**: 003-indexer-read-side
**Spec**: [spec.md](./spec.md) | **Plan**: [plan.md](./plan.md)

How to bring up the cluster locally and verify the acceptance metrics for [#69](https://github.com/cardano-foundation/cardano-node-antithesis/issues/69).

## Prerequisites

- This worktree at `/code/cardano-node-antithesis-issue-69/` (branch `003-indexer-read-side`).
- Daemon image from [`cardano-node-clients#78`](https://github.com/lambdasistemi/cardano-node-clients/issues/78) published to ghcr (or built locally and bind-mounted — see "Daemon not yet published" below).
- `docker`, `docker compose v2`, `jq`, `socat` on the host (the host needs them for the inspection commands below; the containers ship their own).
- `INTERNAL_NETWORK=false` in your env so the bridge has external egress for image pulls. Set `=true` to mirror Antithesis's network shape.

## Bring up

```bash
cd /code/cardano-node-antithesis-issue-69/testnets/cardano_node_master
INTERNAL_NETWORK=false docker compose up -d
```

Expected: every service in `docker compose ps` reaches `healthy` (for those with healthchecks) or `Up` within ~2 minutes on a fresh devnet.

## Verify the indexer is wired

```bash
# 1. Indexer container is healthy
docker compose ps indexer
# STATE should be "running (healthy)"

# 2. Socket file is on the shared volume
docker compose exec indexer ls -la /sock/
# Expect: indexer.sock (srw-...)

# 3. Same socket visible read-only inside tx-generator
docker compose exec tx-generator ls -la /sock/
# Expect: indexer.sock visible

# 4. Direct ready probe from tx-generator's container
docker compose exec tx-generator sh -c \
  'echo "{\"ready\":null}" | socat - UNIX-CONNECT:/sock/indexer.sock | jq .'
# Expect: {"ready":true,"tipSlot":...,"processedSlot":...,"slotsBehind":0}

# 5. Direct snapshot probe for a known funded address
ADDR="$(docker compose exec tx-generator sh -c \
  'cat /utxo-keys/genesis.1.addr.info | jq -r .address')"
docker compose exec tx-generator sh -c \
  "echo '{\"utxos_at\":\"$ADDR\"}' | socat - UNIX-CONNECT:/sock/indexer.sock | jq ."
# Expect: {"utxos":[{"txin":"...#0","txout":"<base16>"}, ...]}
```

## Verify SC-001 (no dust / no Tx failed / no UTxO-still-present)

Let the cluster run for 30 minutes after `tx-generator` has started submitting:

```bash
# Wait for first tx
docker compose logs --follow tx-generator | grep -m1 'Submitted tx'
# Then time 30 minutes from this point
sleep 1800

docker logs tx-generator 2>&1 | \
  grep -cE 'Tx failed|UTxO still present|does not meet the minimum UTxO threshold'
# Expect: 0  (today: ~150+ over 30min)
```

## Verify SC-002 (no GetUTxOByAddress LSQ from consumers)

The relay's tracer logs LSQ events to the shared `tracer` volume. Consumers are `tx-generator` and `asteria-player-*`.

```bash
# Sample one minute's worth of relay LSQ events
docker compose exec tracer-sidecar sh -c \
  'tail -F /opt/cardano-tracer/logs/relay1.example/*.log' \
  | grep -E 'GetUTxOByAddress' \
  | grep -E 'tx-generator|asteria-player' &
SAMPLER=$!
sleep 600   # 10 minutes
kill $SAMPLER

# Expect: no matching lines printed.
```

## Verify SC-003 (submission rate within ±5%)

```bash
# Compute target rate from configured interval
INTERVAL_S=$(docker compose exec tx-generator printenv MIN_SLEEP)   # adjust if you renamed
# (Default is 5..30s random; use mean 17.5s for the budget.)

START=$(date +%s)
COUNT_START=$(docker logs tx-generator 2>&1 | grep -c 'Submitted tx')
sleep 600   # 10 minutes
COUNT_END=$(docker logs tx-generator 2>&1 | grep -c 'Submitted tx')
ELAPSED=$(( $(date +%s) - START ))
ACTUAL=$(( COUNT_END - COUNT_START ))
EXPECTED=$(( ELAPSED * 100 / 1750 ))   # 100/avg_interval_in_centiseconds
echo "Submitted: $ACTUAL; expected ~$EXPECTED (±5%)"
```

## Verify SC-006 (cold start `ready=true` within 1 minute on devnet)

```bash
docker compose down -v
START=$(date +%s)
INTERNAL_NETWORK=false docker compose up -d
until docker compose exec tx-generator sh -c \
  'echo "{\"ready\":null}" | socat - UNIX-CONNECT:/sock/indexer.sock 2>/dev/null | jq -e .ready' \
  >/dev/null 2>&1; do
  sleep 1
done
echo "Indexer ready in $(( $(date +%s) - START )) seconds"
# Expect: < 60 on a healthy devnet host.
```

## Verify SC-007 (warm restart returns to ready)

```bash
START=$(date +%s)
docker compose restart indexer
until docker compose exec tx-generator sh -c \
  'echo "{\"ready\":null}" | socat - UNIX-CONNECT:/sock/indexer.sock 2>/dev/null | jq -e .ready' \
  >/dev/null 2>&1; do
  sleep 1
done
echo "Indexer warm-ready in $(( $(date +%s) - START )) seconds"
# Expect: comparable to cold-start (no persistence in v1; re-indexes from genesis).
```

## Daemon not yet published

If [`cardano-node-clients#78`](https://github.com/lambdasistemi/cardano-node-clients/issues/78) hasn't shipped a published image yet, build the daemon locally and bind-mount the binary into a minimal base image, or override the image with a locally tagged build:

```bash
cd /code/cardano-node-clients-issue-78    # the daemon's worktree
just build-image          # produces local/cardano-utxo-indexer:dev
docker tag local/cardano-utxo-indexer:dev \
  ghcr.io/lambdasistemi/cardano-utxo-indexer:dev
cd -
INTERNAL_NETWORK=false docker compose up -d
```

This path is for local validation only — the merged compose must reference a published image (constitution Principle VII).

## Troubleshooting

- **`indexer` healthcheck loops red**: check `docker logs indexer` for chain-sync errors. If "ChainSyncProtocolError" or socket-permission errors appear, the relay's socket may be unmounted or the `relay1-state:/state:ro` mount is wrong.
- **`tx-generator` logs `IndexerUnavailable: Connection refused`**: the indexer container isn't ready yet (compose `depends_on` should prevent this; if it doesn't, the healthcheck command shipped in the daemon image is wrong).
- **`tx-generator` submits but never refunds**: confirm the active UTxO actually drifts below `REFUND_THRESHOLD_LOVELACE`. If the workload is too slow to drain it, lower `REFUND_TOPUP_LOVELACE` for testing or temporarily set `REFUND_THRESHOLD_LOVELACE=99000000` to force a refund branch in the SDK report.
- **Dust line still appears once after upgrade**: the leftover active UTxO from a pre-upgrade run can be below threshold; tx-generator's first cycle should refund it. If it doesn't, wipe the state with `docker compose down -v`.

## Tear down

```bash
docker compose down -v   # -v also drops indexer-sock
```
