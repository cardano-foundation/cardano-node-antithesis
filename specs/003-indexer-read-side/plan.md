# Implementation Plan: Indexer-Backed Read Side for Test Workloads

**Branch**: `003-indexer-read-side` | **Date**: 2026-04-27 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/003-indexer-read-side/spec.md`
**Parent issue**: [cardano-foundation/cardano-node-antithesis#69](https://github.com/cardano-foundation/cardano-node-antithesis/issues/69)
**External dependency**: [lambdasistemi/cardano-node-clients#78](https://github.com/lambdasistemi/cardano-node-clients/issues/78) — minimal address→UTxO indexer daemon (Unix socket, NDJSON, in-memory)

## Summary

Move the read side of every test workload off `cardano-cli query utxo --address` (LSQ-by-address) onto the small daemon scoped in [`#78`](https://github.com/lambdasistemi/cardano-node-clients/issues/78), and stop tx-generator's dust-and-poll wedge by reshaping its submit loop into a deterministic self-pay chain with a refund actor. This plan covers the **antithesis-side work only**: porting the Python `tx-generator`, wiring the daemon into `testnets/cardano_node_master/docker-compose.yaml`, and giving the asteria-player rebase a clear contract. The daemon itself ships upstream in [`#78`](https://github.com/lambdasistemi/cardano-node-clients/issues/78).

## Technical Context

**Language/Version**:
- tx-generator: Python 3.11 (current `components/tx-generator/Dockerfile` base; client uses stdlib `socket` + `json`, no new deps).
- asteria-player: Haskell (rebase guidance only — implementation lives in the [PR #67](https://github.com/cardano-foundation/cardano-node-antithesis/pull/67) worktree).
- Compose: docker-compose v2 YAML.

**Primary Dependencies**:
- Existing `cardano-cli` (in tx-generator image — used for `transaction build` / `sign` / `submit` only; query usage removed).
- Indexer daemon image from [`cardano-node-clients#78`](https://github.com/lambdasistemi/cardano-node-clients/issues/78), pulled by tag from `ghcr.io/lambdasistemi/...` (final image name + tag picked when daemon ships).
- Antithesis SDK (already imported in `tx_generator.py:17–27`).

**Storage**: None on the antithesis side. Daemon's in-memory index is its concern.

**Testing**:
- Local smoke test via `just smoke-test` per constitution (existing recipe must keep passing).
- New consumer-side unit tests for the NDJSON client (no daemon required — tested against a `socketpair`-mocked server in tests).
- New compose-level integration test: bring the cluster + daemon up, verify the SC-001/SC-002 acceptance metrics over a 5-minute window.

**Target Platform**: Linux containers, docker compose; same as the rest of `testnets/cardano_node_master/`.

**Project Type**: Workload component port + compose wiring. Single-project, no new top-level service in this repo.

**Performance Goals**:
- SC-001: zero `Tx failed` / `UTxO still present` / dust lines in 30 minutes (today: 23 in 5 minutes).
- SC-002: zero `GetUTxOByAddress` LSQ requests from tx-generator and asteria-player containers over a 10-minute window.
- SC-003: submitted-tx count within ±5% of `elapsed / configured_interval`.
- SC-006: indexer daemon `ready=true` within 1 minute on local devnet.

**Constraints**:
- Submission MUST stay on LocalTxSubmission via the existing relay socket (FR-009).
- Compose pattern is shared-volume Unix sockets — no TCP between containers (clarification 3 in spec).
- Constitution principle VII: every image tag in compose must resolve to a commit; bumping tx-generator's tag is part of this PR.
- Constitution principle I (composer-first): tx-generator is a long-running daemon today, not a composer command. This PR preserves that shape; conversion to composer is out of scope for #69. **Documented deviation, see Complexity Tracking.**

**Scale/Scope**:
- ~250 LoC Python rewrite of `tx_generator.py` (similar size to today, fewer code paths).
- ~50 LoC new `indexer_client.py` for the NDJSON Unix-socket protocol.
- ~30 lines of YAML edits in `docker-compose.yaml` (new service + new volume + healthcheck wiring).
- No new top-level directories.

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Note |
|---|---|---|
| I. Composer-first workload | **Deviation** | tx-generator is a long-running compose service, not a composer command. Pre-existing structure; this PR ports the read side and the chain shape but does NOT redesign tx-generator into composer commands. Tracked in Complexity Tracking. |
| II. SDK instrumentation mandatory | PASS | Adds `reachable` on indexer-client init success, `sometimes(refund_triggered)` around the refund branch, `always(active_utxo_above_min)` invariant check before each self-pay tx, `unreachable("indexer_silently_unreachable")` if the client ever falls back to LSQ (defensive — should not fire). |
| III. Short-running commands | N/A | tx-generator is not a composer command. |
| IV. Duration-robust | PASS | Deterministic chain + refund pattern is independent of run duration. |
| V. Realistic workload over synthetic volume | PASS-by-indirection | tx-generator stays synthetic; this PR is the precondition that unblocks the realistic asteria workload (PR [#67](https://github.com/cardano-foundation/cardano-node-antithesis/pull/67)) per User Story 3. |
| VI. Bisect-safe commits | PASS | Vertical commits planned: (1) NDJSON client lib, (2) tx-generator deterministic chain, (3) tx-generator refund actor, (4) compose wiring + image bump. Each compiles, smoke-test passes. |
| VII. Image tag hygiene | PASS | tx-generator's tag in `testnets/cardano_node_master/docker-compose.yaml` bumps to a commit SHA from this branch in commit (4). |

**Gate result**: PASS with one documented deviation (composer-first), justified in Complexity Tracking. Proceed to Phase 0.

## Project Structure

### Documentation (this feature)

```text
specs/003-indexer-read-side/
├── plan.md                 # This file (/speckit.plan output)
├── research.md             # Phase 0 — decisions on tx shape, refund model, client wire
├── data-model.md           # Phase 1 — tx-generator state, NDJSON client contract
├── contracts/
│   ├── indexer-client.md       # NDJSON request/response shapes consumed
│   └── compose-topology.md     # New service + volume + healthcheck contract
├── quickstart.md           # Phase 1 — local bring-up + acceptance verification
├── checklists/
│   └── requirements.md     # Existing spec-quality checklist
└── tasks.md                # Phase 2 output (/speckit.tasks — NOT created here)
```

### Source Code (repository root, files touched by this feature)

```text
components/
├── tx-generator/
│   ├── Dockerfile              # No change (stdlib only)
│   ├── requirements.txt        # No change
│   ├── tx_generator.py         # Rewrite: deterministic chain, refund actor, NDJSON client; remove LSQ queries + post-submit poll
│   └── indexer_client.py       # NEW: ~50 LoC NDJSON-over-Unix-socket client
└── asteria-player/             # Untouched here; rebase on top of this PR is PR #67's work

testnets/cardano_node_master/
├── docker-compose.yaml         # Add `indexer` service, `indexer-sock` volume, healthcheck wiring; mount socket read-only into tx-generator + asteria-player services; bump tx-generator image tag
└── indexer-config.json         # NEW (if needed): daemon config (relay socket path, listen socket path, ready threshold)
```

**Structure Decision**: Single project, two file additions (`indexer_client.py`, optionally `indexer-config.json`), one significant rewrite (`tx_generator.py`), targeted edits to `docker-compose.yaml`. No new top-level directory. Deliberately minimal.

## Complexity Tracking

| Violation | Why Needed | Simpler Alternative Rejected Because |
|---|---|---|
| Constitution Principle I (composer-first): tx-generator stays a non-composer long-running service | Today's tx-generator is a long-running daemon by design ([cardano-node-antithesis#69](https://github.com/cardano-foundation/cardano-node-antithesis/issues/69) treats it as such); the issue's scope is fixing the read-side query pattern and the dust wedge, not redesigning tx-generator into composer commands. | "Convert tx-generator to a composer command in the same PR" rejected because it would expand scope significantly and decouples poorly from #69's required-direction. Tracked as a follow-up; not blocking this PR. |

## Phase 0 — Research

See [research.md](./research.md). Resolved decisions: tx-shape (single self-pay output, change-only build), refund actor signal (active-UTxO threshold T as env var), NDJSON client failure model (raise + halt; no node-fallback), compose healthcheck mechanism (`socat` line-write into `ready` request), tx-confirmation pacing (default off; `await` available for stricter modes).

## Phase 1 — Design & Contracts

- [data-model.md](./data-model.md) — tx-generator's running state (active UTxO, threshold, faucet pool, last-built tx body), the NDJSON client's connection model.
- [contracts/indexer-client.md](./contracts/indexer-client.md) — exact request/response JSON shapes consumed; failure cases.
- [contracts/compose-topology.md](./contracts/compose-topology.md) — the YAML diff (service shape, volume mounts, healthcheck command).
- [quickstart.md](./quickstart.md) — `INTERNAL_NETWORK=false docker compose up -d`; verify `ready=true`; observe LSQ trace; check `docker logs tx-generator`.

## Constitution Check (post-design)

Re-evaluating after Phase 1:
- Principle II (SDK): contracts/indexer-client.md fixes exact assertion sites — PASS.
- Principle VI (bisect-safe): the vertical-commit list above is preserved by the data-model boundaries (client is a separate file from the rewrite, refund logic is a separate function from the chain logic). PASS.
- Principle VII (image tag hygiene): contracts/compose-topology.md mandates a tag bump in the same PR. PASS.

No new violations introduced by Phase 1 design.

## Out-of-band

- Daemon delivery: tracked upstream in [`cardano-node-clients#78`](https://github.com/lambdasistemi/cardano-node-clients/issues/78). This PR carries an empty (placeholder) daemon image tag in compose until the daemon ships; smoke-test gating happens once the upstream image is published.
- Asteria rebase: tracked in [PR #67](https://github.com/cardano-foundation/cardano-node-antithesis/pull/67). The contracts in this plan (`indexer-client.md`, `compose-topology.md`) are the integration surface; that PR consumes them in Haskell.
