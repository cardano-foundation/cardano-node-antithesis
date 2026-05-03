# cardano_node_tx_generator testnet

Iteration testnet for the Haskell `cardano-tx-generator` daemon
([upstream](https://github.com/lambdasistemi/cardano-node-clients)).

The daemon is exercised by the standard 3-pool +2-relay Cardano cluster
(same compose shape as `cardano_node_master`) plus the sidecar /
tracer-sidecar / log-tailer / tracer instrumentation.

## Why a separate testnet

`cardano_node_master` is the production-baseline testnet and must not
be modified on feature branches. tx-generator iteration — pin bumps,
composer-script changes, fault-injection scope — happens here until
the reconnect-resilience surface is stable, at which point the change
is promoted to master in a separate, explicitly reviewed PR.

## Image set

Mirrors `cardano_node_master`'s image set as of the branch base:

| Component          | Source                                                     |
|--------------------|------------------------------------------------------------|
| cardano-node       | `ghcr.io/intersectmbo/cardano-node@sha256:…`               |
| cardano-tracer     | `ghcr.io/intersectmbo/cardano-tracer@sha256:…`             |
| configurator       | `…/cardano-node-antithesis/configurator@sha256:…`          |
| sidecar            | `…/sidecar:65039df`                                         |
| tracer-sidecar     | `…/tracer-sidecar@sha256:…`                                |
| log-tailer         | `…/log-tailer@sha256:…`                                    |
| **tx-generator**   | `…/tx-generator:<branch-sha>` (active, not parked)         |

The tx-generator pin is in `components/tx-generator/flake.nix` and
carries upstream PRs #105/#110/#114/#115/#116/#117/#118.

## Running locally

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
