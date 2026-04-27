# Implementation Plan: yaci-store as Antithesis N2N consumer

**Branch**: `004-yaci-store` | **Date**: 2026-04-27 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/004-yaci-store/spec.md`
**Issue**: [#75](https://github.com/cardano-foundation/cardano-node-antithesis/issues/75)

## Summary

Add an upstream `bloxbean/yaci-store` container as a long-running N2N consumer in `testnets/cardano_node_master/docker-compose.yaml`. Configure it for the testnet's protocol magic (42) and custom byron/shelley/alonzo/conway genesis files emitted by the existing configurator. Use H2 in-memory storage (no Postgres sidecar). Pin by direct upstream digest (no mirror). Verify SIGTERM behaviour and that a 1h Antithesis run finds no new findings versus the prior `main` baseline.

## Technical Context

**Language/Version**: N/A (Java/Spring Boot upstream image, consumed as-is — no source code changes in this repo).
**Primary Dependencies**: Docker Compose (existing), `docker.io/bloxbean/yaci-store:2.0.0` (upstream image, pinned by `@sha256` digest), the existing configurator's genesis output mounted from `p1-configs:/configs:ro`.
**Storage**: H2 in-memory (`spring.datasource.url=jdbc:h2:mem:mydb`). Tmpfs scratch if the container needs writable scratch.
**Testing**: Local — `just up cardano_node_master`, `just smoke-test`, manual confirmation that yaci-store reaches relay tip via container logs (`/actuator/health` if reachable; otherwise log lines). CI — existing `publish-images` (no change needed: the image is upstream-pinned, not built here). Integration — Antithesis `duration=1h` run on this branch; finding count compared to `main` baseline.
**Target Platform**: Linux x86_64 inside docker-compose; Antithesis runners.
**Project Type**: Testnet workload addition — single docker-compose service plus one config file.
**Performance Goals**: yaci-store reaches relay tip slot within 5 minutes of the network producing its first block (SC-001). Embedded H2 must not OOM the container under a 1h run.
**Constraints**: bind mounts must live next to `docker-compose.yaml` (Antithesis cannot resolve relative paths outside the compose dir); image must be content-addressed (no `:latest` / `:dev`); composer-first principle does not apply here (yaci-store is a long-running service, not a composer command — see Constitution Check below); SIGTERM must yield exit 0 or be absorbed by the harness.
**Scale/Scope**: Single new service. No new code components. Config file (~30 lines) + compose stanza (~20 lines).

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Composer-first workload | **Documented deviation** | yaci-store is a long-running N2N service, not a Composer command. Same shape as existing `oura`, `tx-generator`, `sidecar`, `tracer` services that live next to composer commands rather than under `components/<name>/composer/`. The composer-first principle binds the **workload-driving** services; yaci-store is a passive consumer for protocol coverage. Tracked in Complexity Tracking. |
| II. SDK instrumentation is mandatory | **N/A** | This principle binds composer commands. yaci-store does not run composer commands and therefore has nothing to instrument. If we later add an `eventually_*` or `anytime_*` command that interrogates yaci-store, that command must satisfy this principle — out of scope here. |
| III. Short-running commands | **N/A** | No new composer commands in this feature. |
| IV. Duration-robust | **Pass** | yaci-store runs for the full duration; nothing here depends on a specific duration. |
| V. Realistic workload over synthetic volume | **Pass** | yaci-store is a real Cardano-ecosystem consumer (Spring Boot indexer used by many dApps and explorers), not synthetic traffic. |
| VI. Bisect-safe commits | **Pass** | Single feature commit (compose stanza + config file + spec/plan/tasks docs). `just smoke-test` must pass after the commit. |
| VII. Image tag hygiene | **Pass** | Upstream image is pinned by `@sha256:<digest>`. Constitution principle VII binds **in-repo `ghcr.io/cardano-foundation/cardano-node-antithesis/<name>` images**, which yaci-store is not — same exemption as `cardano-node`, `cardano-tracer`, `oura`, `txpipe/oura`, etc. |
| Custom testnet boundaries | **Pass** | yaci-store consumes the private testnet's relay over N2N with its own genesis files. Zero mainnet dependency. |
| Minimal sidecar image | **N/A** | yaci-store has its own image. No new shell scripts are placed in the sidecar. |
| Composer smoke-tests run locally | **N/A** | No new composer commands. |
| Hard gate: findings_new ≤ baseline | **Owned by SC-002 / FR-008** | Verified pre-merge per the verify-before-merge workflow in the constitution's Development Workflow section. |

**Verdict**: One documented deviation (Principle I, "yaci-store is not a composer command"), one explicit exemption (Principle VII, "upstream image, not in-repo component"). Both are precedented by services already in `docker-compose.yaml`. No unjustified violations.

## Project Structure

### Documentation (this feature)

```text
specs/004-yaci-store/
├── plan.md              # This file
├── spec.md              # /speckit.specify output
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output (config schema + container shape)
├── quickstart.md        # Phase 1 output (bring-up + verification commands)
├── contracts/
│   └── compose-topology.md   # yaci-store service shape, volumes, dependencies
├── checklists/
│   └── requirements.md
└── tasks.md             # /speckit.tasks output (NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
testnets/
└── cardano_node_master/
    ├── docker-compose.yaml          # +1 service stanza: yaci-store
    ├── yaci-store-config.properties # NEW: bind-mounted into yaci-store
    │                                #     contains store.cardano.* + datasource
    └── ...                          # (existing files unchanged)
```

No `components/` directory is added. yaci-store is consumed from upstream Docker Hub.

**Structure Decision**: Add one service to the existing `cardano_node_master` testnet. No new component built in this repo. Configuration lives next to `docker-compose.yaml` per Antithesis bind-mount rules. The feature is intentionally minimal — the cost of complexity here (extra components/, custom image build, mirror step) buys nothing for a passive N2N coverage consumer.

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| Principle I deviation: yaci-store is not driven by Test Composer | yaci-store's role is protocol-coverage exposure of a Cardano-ecosystem consumer to fault injection, not goal-driven workload. Wrapping the indexer's startup behind a composer command would be ceremony with no semantic benefit; the composer's value (SDK assertions, scheduling primitives) does not apply to a service whose only behaviour is "stay connected and follow the chain". | Wrapping yaci-store behind a composer command (e.g. `singleton_driver_yaci_store_runner`) was considered. Rejected because (a) the composer prefixes are designed for short-running validation/driver commands, not indefinitely-running services; (b) no SDK assertion has a meaningful target on a long-running JVM service; (c) the precedent set by `oura`, `tracer`, `tx-generator`, and `sidecar` is to run them as native compose services. |
