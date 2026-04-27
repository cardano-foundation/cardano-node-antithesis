# Contract: Compose Topology

**Feature**: 003-indexer-read-side
**Spec**: [../spec.md](../spec.md) | **Plan**: [../plan.md](../plan.md) | **Data Model**: [../data-model.md](../data-model.md)
**File touched**: [`testnets/cardano_node_master/docker-compose.yaml`](../../../testnets/cardano_node_master/docker-compose.yaml)

This is the YAML diff this PR introduces. The shape (not the literal bytes) is the contract: a follow-up to a sibling testnet that wants the same indexer follows the same shape.

## Adds

### A new `indexer` service

```yaml
  indexer:
    image: ghcr.io/lambdasistemi/cardano-utxo-indexer:<tag>   # final image name from #78
    container_name: indexer
    hostname: indexer.example
    command: >
      --relay-socket /state/node.socket
      --listen /sock/indexer.sock
    volumes:
      - relay1-state:/state:ro
      - indexer-sock:/sock
    healthcheck:
      test:
        - "CMD-SHELL"
        - 'echo ''{"ready":null}'' | socat - UNIX-CONNECT:/sock/indexer.sock | jq -e .ready'
      interval: 5s
      timeout: 3s
      retries: 60
      start_period: 10s
    restart: always
    depends_on:
      relay1:
        condition: service_started
```

Notes:
- Mounts the same `relay1-state` volume that today's `tx-generator` and `oura` mount. Same N2C socket, read-only.
- Owns the new `indexer-sock` volume read-write. Consumers mount it read-only.
- The healthcheck uses the wire from [contracts/indexer-client.md](./indexer-client.md) REQ-3. `socat` and `jq` are expected in the daemon image (the daemon is allowed to ship a small amount of tooling for self-healthcheck — constitution's "minimal sidecar image" rule applies to *sidecar* composer-command containers, not to service containers like this one).

### A new `indexer-sock` volume

```yaml
volumes:
  # ... existing ...
  indexer-sock:
```

Ephemeral. Holds exactly one file: `indexer.sock`. Cleared on `docker compose down -v`.

## Modifies

### `tx-generator` service

```yaml
  tx-generator:
    image: ghcr.io/cardano-foundation/cardano-node-antithesis/tx-generator:<commit-sha-from-this-branch>   # bumped per constitution VII
    # ... existing fields ...
    environment:
      # existing:
      CARDANO_NODE_SOCKET_PATH: /state/node.socket
      NETWORK_MAGIC: "42"
      # added:
      INDEXER_SOCKET_PATH: /sock/indexer.sock
      REFUND_THRESHOLD_LOVELACE: "5000000"
      REFUND_TOPUP_LOVELACE: "100000000"
      TX_GENERATOR_PACING: "none"
      INDEXER_RETRY_BACKOFF: "5"
    volumes:
      # existing:
      - relay1-state:/state:ro
      - p1-configs:/configs:ro
      - utxo-keys:/utxo-keys:ro
      # added:
      - indexer-sock:/sock:ro
    depends_on:
      # existing:
      relay1:
        condition: service_started
      # added:
      indexer:
        condition: service_healthy
```

Notes:
- Image tag bump is mandatory per constitution principle VII. The actual SHA is filled in by the final commit of the branch (vertical commit 4 in [plan.md](../plan.md)).
- `relay1-state` mount stays read-only — submission still uses `cardano-cli transaction submit` over the relay's Unix socket (FR-009). Only the read side moves.
- Refund threshold + top-up env vars expose the invariants from [research.md R-002](../research.md). Defaults match the data model.

### Asteria services (`asteria-bootstrap`, `asteria-player-1`, `asteria-player-2`)

Identical mount + env additions as tx-generator (the `INDEXER_SOCKET_PATH` env, the `indexer-sock:/sock:ro` volume, the `depends_on: indexer: service_healthy`). Image tag for asteria-player is **not** bumped in this PR — the asteria-player binary's actual port to the indexer happens in [PR #67](https://github.com/cardano-foundation/cardano-node-antithesis/pull/67); this PR only wires the volume + env so PR [#67](https://github.com/cardano-foundation/cardano-node-antithesis/pull/67) inherits them on rebase.

## Does not change

- Existing volumes (`p1-configs`, `p2-configs`, `p3-configs`, `relay1-state`, `relay2-state`, `utxo-keys`, `tracer`, `asteria-sdk`).
- Existing services other than tx-generator / asteria-* (`configurator`, `tracer`, `p1`, `p2`, `p3`, `relay1`, `relay2`, `oura`, `sidecar`, `tracer-sidecar`, `log-tailer`).
- The `networks.default` block (`internal: ${INTERNAL_NETWORK}` toggle untouched). The indexer never exposes a TCP port; the only inter-container channel is the shared `indexer-sock` volume.

## Acceptance against this contract

A docker compose `config` (lint) + `up -d` smoke run on a clean machine MUST:

1. Start `indexer` after `relay1` is up.
2. Reach `indexer` healthy within `start_period + retries × interval = 10 + 60×5 = 5m10s` worst case (target [SC-006](../spec.md): `ready=true` within 1 minute on devnet).
3. Start `tx-generator` only after `indexer` is healthy.
4. `docker compose exec indexer ls /sock/indexer.sock` shows the socket file.
5. `docker compose exec tx-generator ls /sock/indexer.sock` shows the same socket file (verifying the shared-volume mount).
6. `docker logs indexer` shows N2C chain-sync events; `docker logs tx-generator` shows successful `INDEXER_READY` SDK signal followed by tx submissions.

## Image hygiene

Per constitution Principle VII, the tx-generator image tag bump in this PR resolves to a commit on this branch that contains both the rewritten `tx_generator.py` and the new `indexer_client.py`. `publish-images` workflow on push rebuilds and pushes the image; the SHA in compose matches the commit. `merge-guard` enforces this on merge.
