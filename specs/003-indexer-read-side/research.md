# Research: Indexer-Backed Read Side for Test Workloads

**Feature**: 003-indexer-read-side
**Date**: 2026-04-27
**Spec**: [spec.md](./spec.md) | **Plan**: [plan.md](./plan.md)

Resolves the open implementation questions left by the spec.

## R-001 — Transaction shape: single-output self-pay, change-only

**Decision**: Each self-pay transaction has exactly one output: the change output to the sender's address. No `--tx-out` flags are passed; `cardano-cli transaction build --change-address <addr>` produces one output at index 0.

**Rationale**:
- The deterministic chain (FR-003) needs the next `TxIn` to be unambiguous from the previous tx body. With a single change output, the next `TxIn` is `(txid, 0)` — point-equality.
- Today's `tx_generator.py:170–179` builds with N random extra `--tx-out` flags producing N+1-output transactions. That is what makes the next-`TxIn` non-deterministic and is the proximate cause of the dust drift (each split shaves min-UTxO + safety margin off the active leg).
- The constitution's preference for *realistic workload* (Principle V) puts the load-bearing test signal in asteria-player; tx-generator's value is "keep submitting under fault injection", not "produce diverse UTxO topologies". Single-output is sufficient for that.

**Alternatives considered**:
- **Splitter-then-loop** (Q1 option D from clarifications): splits the funded address into K parallel UTxOs at startup, runs K independent self-pay chains in round-robin. Rejected — more complex than the user-accepted refund-actor model (Q1 option B), and it doesn't fix dust, only delays it.
- **Keep multi-output txs but pin change index**: `cardano-cli build` always appends the change output last; we could chain off `(txid, len(tx_outs)-1)`. Rejected — non-determinism in `len(tx_outs)` requires reading the tx body back, and it's a strictly bigger code surface than "no extras, change is index 0".

## R-002 — Refund actor signal: active-UTxO threshold T

**Decision**: tx-generator builds a refund transaction when the active UTxO's lovelace value drops below a configured threshold `T = REFUND_THRESHOLD_LOVELACE` (env var, default 5_000_000 = 5 ADA). The refund transaction consumes the active UTxO plus one faucet UTxO (held under the same key) and produces a single replenished active UTxO with target value `T_top = REFUND_TOPUP_LOVELACE` (env var, default 100_000_000 = 100 ADA). The next self-pay continues off `(refund_txid, 0)`.

**Rationale**:
- Threshold-driven (rather than fixed-N) is duration-robust (constitution IV): a 1-hour run and a 3-hour run both refund based on the actual draw-down rate of the active UTxO, no schedule to tune.
- Faucet UTxOs are discovered the same way as the initial active UTxO — `utxos_at` on the sender address — and any UTxO at the same address with `lovelace ≥ T_top + fees` is eligible. There is no separate "faucet address" concept needed; the existing genesis UTxOs at the funded address are the faucet pool.
- Default `T = 5 ADA` keeps the active leg comfortably above the protocol min-UTxO (~1 ADA) plus margin for two-output ledger costs and a few txs of fee headroom.
- Default `T_top = 100 ADA` gives 95 ADA of usable lovelace per refund cycle; with ~0.2 ADA per self-pay tx (rough order), that's ~475 self-pays per refund — refunds are rare (a few per hour at the default tx interval), keeping their share of submission load negligible.

**Alternatives considered**:
- **Submit refund only on next-self-pay-build-failure**: refund triggered when `build` fails with min-UTxO. Rejected — failure-driven means a noisy log entry per refund and a wasted build attempt; threshold-driven is clean.
- **Refund as a separate restart-the-process step**: kill the active UTxO, spawn a new `genesis.N` chain. Rejected — the docker compose `restart: always` posture would do this for free at the cost of churn; an in-process refund keeps the chain continuous.
- **No refund, allow dust drift**: not viable — kills the workload (the bug we're fixing).

## R-003 — NDJSON client failure model: surface and pause, never fall back

**Decision**: The `indexer_client.py` module raises a `IndexerUnavailable` exception on any of: connection refused, EOF before a full response line, JSON parse error, `{"error": ...}` response from the daemon. Callers in `tx_generator.py` catch `IndexerUnavailable` at exactly one place (the main submit loop), log at WARNING, sleep `INDEXER_RETRY_BACKOFF` seconds (env var, default 5), and retry — they MUST NOT fall back to `cardano-cli query utxo --address`. There is no other catch site for `IndexerUnavailable`.

**Rationale**:
- Spec FR-006 mandates "surface clear error and pause submission, NOT fall back to direct node queries". Codifying this as a single named exception caught in exactly one place makes that property structurally enforceable (rather than a recurring `except Exception` worry).
- Antithesis SDK assertion `unreachable("indexer_silently_unreachable")` is fired in the default `except Exception` arm of the submit loop — i.e. if any other exception type ever masks an indexer failure, the SDK report flags it.

**Alternatives considered**:
- **Library-side retry with backoff**: client retries internally before raising. Rejected — hides the daemon-down condition from the workload-level observability; the submit loop is already a retry loop, no need for double layering.
- **Treat indexer-unavailable as a healthcheck signal**: container exits non-zero, compose restarts. Rejected — that's a sledgehammer; consumers should pause and recover, not crash, because the indexer's restart cadence under fault injection is exactly what we're testing through.

## R-004 — Compose healthcheck: `socat` line-write into `ready` request

**Decision**: The `indexer` service's docker compose `healthcheck.test` is a small inline command that writes one NDJSON request line into the daemon's Unix socket and asserts `ready=true`:

```yaml
healthcheck:
  test:
    - "CMD-SHELL"
    - 'echo ''{"ready":null}'' | socat - UNIX-CONNECT:/sock/indexer.sock | jq -e .ready'
  interval: 5s
  timeout: 3s
  retries: 60
  start_period: 10s
```

`tx-generator` and `asteria-player` services declare `depends_on: indexer: { condition: service_healthy }` so they don't start until ready.

**Rationale**:
- `socat` is already in the testnet's tooling reach (used by other compose services; if not, a tiny helper image is acceptable — TBD when the daemon image lands).
- Constitution's "Minimal sidecar image" rule (`coreutils + bash + jq + cardano-cli`) applies to *sidecar* composer commands, not to the indexer container itself. The indexer container is allowed to ship `socat` (or expose a CLI subcommand for self-healthcheck) as part of its image.
- The `ready` request shape is defined in [contracts/indexer-client.md](./contracts/indexer-client.md); same wire as the consumer client.

**Alternatives considered**:
- **HTTP `/ready` endpoint over TCP**: rejected per Q3 in spec clarifications — this compose has no precedent for service-to-service TCP, and it widens the deterministic-replay surface under Antithesis.
- **TCP-only-for-healthcheck**: rejected on grounds of consistency; one transport everywhere.
- **No healthcheck**: rejected — consumers would race the daemon's catch-up.

## R-005 — Tx-confirmation pacing: default off, `await` available

**Decision**: The default tx-generator loop submits, advances to the next deterministic `TxIn` from the just-built tx body, and goes to the next iteration without calling `await` on the daemon. A `TX_GENERATOR_PACING=await` env var switches in a stricter mode that calls `await` on `(txid, 0)` after each submission with a configurable timeout (env `TX_GENERATOR_AWAIT_TIMEOUT_SECONDS`, default 60).

**Rationale**:
- The deterministic chain assumption is "the previous tx will land or be rejected; either way the next-`TxIn` resolution is correct". If it lands, the chain continues. If it's rejected, the next `build` will fail and the existing `IndexerUnavailable` / build-failure path re-syncs via `utxos_at` (FR-011, User Story 2 scenario 3). There is no functional need to wait between submissions in steady state.
- `await` is available for outer drivers (Antithesis composer commands) that need a "submission landed" gate before doing something else — exposing this without making it the default avoids penalising tx/sec in normal operation.

**Alternatives considered**:
- **Always await**: rejected — adds a synchronous round-trip per tx for no functional gain; lowers achievable tx/sec.
- **Never expose await from tx-generator**: rejected — `await` is the canonical way for an outer test driver to gate on tx confirmation; cutting it off at the consumer boundary forces drivers to invent their own.

## R-006 — Image dependency: placeholder until upstream daemon ships

**Decision**: The indexer service in `docker-compose.yaml` is added in this PR with a placeholder image tag (e.g. `ghcr.io/lambdasistemi/cardano-utxo-indexer:dev` or whatever name [`#78`](https://github.com/lambdasistemi/cardano-node-clients/issues/78) settles on). On a clean `docker compose up -d`, that container will fail to pull until the upstream image is published. The smoke-test gate (constitution §"Composer smoke-tests run locally before every push") is honoured by:
1. Running smoke-test against a local daemon binary (built from the [`#78`](https://github.com/lambdasistemi/cardano-node-clients/issues/78) branch) bind-mounted into the container until publish, OR
2. Holding the final commit (the image-tag bump) until the upstream image is published.

The PR does not merge until path 2 is true (constitution VII). `publish-images` workflow on `cardano-node-antithesis` will then green-light Antithesis runs.

**Rationale**: Decoupling the antithesis-side rewrite from the daemon's publish cadence lets us land the consumer-side commits first (vertical commits 1–3) and finalise the compose wiring in commit 4 once the daemon is published.

**Alternatives considered**:
- **Wait for [`#78`](https://github.com/lambdasistemi/cardano-node-clients/issues/78) to ship before opening this PR**: rejected — keeps work serialised when consumer-side commits can be reviewed in parallel.
- **Vendor the daemon source into this repo**: rejected — duplicates code, breaks upstream's release cadence, and the daemon is explicitly a `cardano-node-clients` deliverable per the spec's clarifications.

## R-007 — Asteria-player rebase contract (informational, no code in this PR)

**Decision**: The asteria-player port to the indexer happens in [PR #67](https://github.com/cardano-foundation/cardano-node-antithesis/pull/67), not here. This plan provides:
- The Haskell-side contract: connect to `/sock/indexer.sock` via Unix domain socket, NDJSON line protocol, same request shapes as the Python client (`utxos_at`, `await`, `ready`). See [contracts/indexer-client.md](./contracts/indexer-client.md).
- The compose wiring: asteria-player services already share the same `indexer-sock` volume mount (added in this PR's compose changes); their entrypoints will call the new client lib once PR [#67](https://github.com/cardano-foundation/cardano-node-antithesis/pull/67) implements it.
- The acceptance gate: SC-005 (asteria-spawn-v2 acceptance for User Story 1 of [`specs/phase1-asteria-gatherer/spec.md`](../phase1-asteria-gatherer/spec.md) passes end-to-end with zero LSQ-by-address calls from the asteria-player container).

**Rationale**: keeps PRs small and reviewable; the antithesis-side compose wiring + tx-generator port can land independently of the Haskell rewrite.

## Open items

None blocking implementation. The daemon image tag (R-006) is the only deferred decision and lands when [`cardano-node-clients#78`](https://github.com/lambdasistemi/cardano-node-clients/issues/78) ships its first publish.
