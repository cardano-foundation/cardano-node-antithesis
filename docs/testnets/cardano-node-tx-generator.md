# Cardano Node Tx-generator

Sibling iteration testnet for the Haskell `cardano-tx-generator` daemon.
Mirrors the [Cardano Node Master](cardano-node-master.md) image set
1:1 (same cardano-node digests, configurator, sidecar, tracer-sidecar,
log-tailer) plus the tx-generator service active (not parked).

## Why a separate testnet

`cardano_node_master` is the production-baseline testnet on the
scheduled Antithesis cron. Feature work on the tx-generator daemon —
upstream pin bumps, composer-script changes, reconnect-resilience
iteration — must not contaminate the master baseline. This testnet
is the iteration surface; the daemon is promoted to master in a
separate, explicitly-reviewed PR once it's perfectly stable.

## Image set

Identical to master's, byte-for-byte:

| Service          | Image                                                            |
|------------------|------------------------------------------------------------------|
| p1, relay1       | `ghcr.io/intersectmbo/cardano-node@sha256:c3cbc5aa…2c4b2`        |
| p2, relay2       | `ghcr.io/intersectmbo/cardano-node@sha256:45857be8…42271`        |
| p3               | `ghcr.io/intersectmbo/cardano-node@sha256:5ae211f9…d19f`         |
| tracer           | `ghcr.io/intersectmbo/cardano-tracer@sha256:da628263…28efe`      |
| configurator     | `…/configurator@sha256:6e6ba428…6a34d`                          |
| sidecar          | `…/sidecar:65039df`                                              |
| tracer-sidecar   | `…/tracer-sidecar@sha256:8474b148…0b5c689`                      |
| log-tailer       | `…/log-tailer@sha256:a5d23c42…df56484`                          |
| **tx-generator** | `…/tx-generator:<branch-sha>` (see [Tx-generator](../components/tx-generator.md)) |

## Network topology

Same 3-pool +2-relay shape as master (p1 ↔ p2 ↔ p3 ring; relay1 +
relay2 connect to all producers). The tx-generator container connects
to **relay1** via N2C over a single multiplexed bearer (ChainSync +
LocalStateQuery + LocalTxSubmission).

Network name is `cardano-node-tx-generator-testnet` (distinct from
master's `cardano-node-testnet`) so both testnets can coexist
side-by-side on the same docker daemon during local A/B work.

## Running locally

```bash
just up testnet=cardano_node_tx_generator
just logs tx-generator testnet=cardano_node_tx_generator
just down testnet=cardano_node_tx_generator
```

Or via the smoke-test wrapper used by CI:

```bash
INTERNAL_NETWORK=true scripts/smoke-test.sh cardano_node_tx_generator 600
```

## Antithesis dispatch

```bash
gh workflow run "Antithesis on cardano-node testnet" \
    --ref feat/tx-generator-n2c-reconnect-bump \
    -f test=cardano_node_tx_generator \
    -f duration=1
```

The workflow's `test` input flows into `TESTNET` (per `e3b09a0` —
the prior hardcoded `cardano_node_master` env silently ignored the
input).

## Image build pipeline

The repo-wide
[`publish-images.yaml`](https://github.com/cardano-foundation/cardano-node-antithesis/blob/main/.github/workflows/publish-images.yaml)
workflow scans every `testnets/*/docker-compose.yaml`
([`81f3bf1`](https://github.com/cardano-foundation/cardano-node-antithesis/commit/81f3bf1))
and rebuilds whichever images are missing in the registry, so this
testnet's tx-generator image is picked up automatically — no per-testnet
publish workflow is needed.

The smoke step in that workflow targets `cardano_node_master`; this
testnet's smoke is exercised by the dedicated 1h Antithesis dispatch
above and locally via:

```bash
INTERNAL_NETWORK=true scripts/smoke-test.sh cardano_node_tx_generator 600
```

`smoke-test.sh` has a tx-generator probe built in, gated on the
service being present in the resolved compose — driving 5 transacts
end-to-end and asserting `populationSize > 1`.

## Promotion to master

When the daemon is stable, promotion is a separate PR that touches
*only* `testnets/cardano_node_master/docker-compose.yaml`
(un-park the tx-generator service, point at the latest published
tag). This iteration testnet remains as the surface for further
iteration after promotion.
