# cardano_node_tx_generator testnet

Iteration testnet for the Haskell `cardano-tx-generator` daemon
([upstream](https://github.com/lambdasistemi/cardano-node-clients)).

The daemon is exercised by the 3-pool +2-relay Cardano cluster used by
the 2026-05-05 validation baseline plus the sidecar /
tracer-sidecar / log-tailer / tracer instrumentation.

## Why a separate testnet

`cardano_node_tx_generator` remains the iteration surface for
tx-generator pin bumps, composer-script changes, and fault-injection
scope. Promotion to `cardano_node_master` requires an explicit evidence
run showing that the tx-generator image matches the current master image
set for all shared services and that the composer commands terminate
cleanly under faults.

## Image set

The validated promotion run used the same shared image set as the
completed `cardano_node_master` baseline run from 2026-05-05 12:42 UTC.
Only the workload images differed: this testnet added
`tx-generator:69bf815`, while master also included `asteria-game:f7ce4a2`.

| Component          | Source                                                     |
|--------------------|------------------------------------------------------------|
| cardano-node       | `ghcr.io/intersectmbo/cardano-node@sha256:…`               |
| cardano-tracer     | `ghcr.io/intersectmbo/cardano-tracer@sha256:…`             |
| configurator       | `…/cardano-node-antithesis/configurator@sha256:…`          |
| sidecar            | `…/sidecar:1ff6913`                                        |
| tracer-sidecar     | `…/tracer-sidecar@sha256:…`                                |
| log-tailer         | `…/log-tailer@sha256:…`                                    |
| **tx-generator**   | `…/tx-generator:69bf815` (active, not parked)              |

The tx-generator pin is in `components/tx-generator/flake.nix` and
carries upstream PRs #105/#110/#114/#115/#116/#117/#118.

## Promotion evidence

The 1h faults-enabled Antithesis run
`9352ad089c67523bc2ba2c14d1d18b5b39b24f49797c640214eaf375abf74944`
finished with 40/40 properties passing and no new, ongoing, resolved,
or rare findings. The previously failing composer commands all returned
zero across the run:

| Command | Passed | Failed |
|---------|-------:|-------:|
| `tx-generator/parallel_driver_transact.sh` | 5,288 | 0 |
| `tx-generator/parallel_driver_refill.sh` | 3,506 | 0 |
| `tx-generator/eventually_population_grew.sh` | 1,448 | 0 |
| `tx-generator/finally_pressure_summary.sh` | 413 | 0 |

Measured pressure from the run:

| Metric | Value |
|--------|------:|
| Transact task starts | 5,709 |
| Refill task starts | 4,597 |
| Population-growth assertions | 166 |
| Refill-landed assertions | 43 |
| Pressure-summary assertions | 413 |
| Max concurrent composer commands | 12 |

## Running locally

```bash
INTERNAL_NETWORK=true scripts/smoke-test.sh cardano_node_tx_generator 600
```

## Antithesis dispatch

```bash
gh workflow run "Antithesis on cardano-node testnet" \
    --ref chore/tx-generator-match-master-images \
    -f test=cardano_node_tx_generator \
    -f duration=1
```
