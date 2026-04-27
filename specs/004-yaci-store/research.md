# Phase 0 Research: yaci-store as Antithesis N2N consumer

**Feature**: 004-yaci-store
**Date**: 2026-04-27

This document consolidates the research that resolved every NEEDS CLARIFICATION raised by `spec.md` and `plan.md`. Each entry follows: Decision / Rationale / Alternatives.

---

## R-001: Image source and version

**Decision**: Use `docker.io/bloxbean/yaci-store:2.0.0`, pinned in `docker-compose.yaml` by `@sha256:<digest>`. The exact digest is captured in compose at the time of the implementation commit and recorded in `quickstart.md`.

**Rationale**: `2.0.0` (released 2026-02-02) is the latest stable upstream tag on Docker Hub. `2.1.0-pre3` exists but is a pre-release and explicitly marked unstable. The `bloxbean/yaci-store` Dockerfile target builds `yaci-store.jar` and starts it with `java -jar yaci-store.jar`, exposing port 8080 (Spring Actuator) — exactly the shape we want for a passive consumer.

**Alternatives considered**:
- `bloxbean/yaci-store-ledger-state` / `bloxbean/yaci-store-utxo-indexer`: heavier indexers; not needed for pure N2N protocol coverage.
- Build a thin wrapper image under `components/yaci-store/`: would route yaci-store through the repo's `publish-images` workflow, but adds a build pipeline, a `Dockerfile`, and a release surface that buys nothing — the upstream image already accepts the configuration we need via env vars and bind mounts. Rejected as overengineering.
- Pin `latest` or a floating major: violates [No `:dev` in compose](https://github.com/cardano-foundation/cardano-node-antithesis/issues) hygiene and breaks Antithesis runners that resolve digests at workflow dispatch.

**Sources**:
- [bloxbean/yaci-store releases](https://github.com/bloxbean/yaci-store/releases)
- [bloxbean/yaci-store Dockerfile](https://github.com/bloxbean/yaci-store/blob/main/Dockerfile)
- [Docker Hub: bloxbean/yaci-store tags](https://hub.docker.com/r/bloxbean/yaci-store/tags)

---

## R-002: Image is **not** mirrored by the repo's publish-images workflow

**Decision**: yaci-store's image reference in compose stays `docker.io/bloxbean/yaci-store@sha256:<digest>`. No mirror step. No registry indirection. Same pattern as the existing `cardano-node`, `cardano-tracer`, `txpipe/oura` references.

**Rationale**: Reading [`scripts/push-cardano_node_master_images.sh`](https://github.com/cardano-foundation/cardano-node-antithesis/blob/main/scripts/push-cardano_node_master_images.sh) shows the script extracts only entries matching `ghcr.io/cardano-foundation/cardano-node-antithesis/<name>` and rebuilds them from this repo's `components/<name>/` directory. Anything else in compose is a passthrough reference Antithesis pulls directly. Constitution Principle VII binds in-repo components only.

**Alternatives considered**:
- Mirror Docker Hub → GHCR via a new step in `push-cardano_node_master_images.sh`: would let us digest-pin under our namespace, but introduces drift risk (our mirror lagging upstream releases) and a new piece of CI surface. Rejected — direct upstream pinning by digest gives us byte-for-byte determinism.

**Sources**:
- [push-cardano_node_master_images.sh](https://github.com/cardano-foundation/cardano-node-antithesis/blob/main/scripts/push-cardano_node_master_images.sh)
- [.github/workflows/publish-images.yaml](https://github.com/cardano-foundation/cardano-node-antithesis/blob/main/.github/workflows/publish-images.yaml)

---

## R-003: Custom-magic / custom-genesis configuration surface

**Decision**: Pass configuration via environment variables on the yaci-store container. Mount the configurator's emitted genesis files read-only from `p1-configs:/configs:ro`. Set, at minimum:

```
STORE_CARDANO_HOST=relay1.example
STORE_CARDANO_PORT=3001
STORE_CARDANO_PROTOCOL_MAGIC=42
STORE_CARDANO_BYRON_GENESIS_FILE=/configs/configs/byron-genesis.json
STORE_CARDANO_SHELLEY_GENESIS_FILE=/configs/configs/shelley-genesis.json
STORE_CARDANO_ALONZO_GENESIS_FILE=/configs/configs/alonzo-genesis.json
STORE_CARDANO_CONWAY_GENESIS_FILE=/configs/configs/conway-genesis.json
SPRING_DATASOURCE_URL=jdbc:h2:mem:mydb
```

Do **not** set `STORE_CARDANO_DEVKIT_NODE` (that flag is for yaci-devkit, not a real cardano-node).

**Rationale**: All these properties are first-class in upstream `config/application.properties`. Spring Boot maps `store.cardano.protocol-magic` → `STORE_CARDANO_PROTOCOL_MAGIC` automatically (uppercase + dot/dash → underscore). The upstream `application-devkit.properties` is the canonical example for custom-magic deployments and uses exactly this shape. Env-var injection avoids shipping a properties file.

**Open item**: The exact filenames the configurator emits (`byron-genesis.json` vs `byron.json` etc.) must be confirmed at implementation time by inspecting `components/configurator/` output. The compose entry is the single source of truth — it points env vars at the real paths once known.

**Alternatives considered**:
- Mount a full `application.properties` next to compose: works but bloats the repo with another config file. Env vars are clearer in compose and surface every knob in one place. Rejected on simplicity grounds.

**Sources**:
- [config/application.properties](https://github.com/bloxbean/yaci-store/blob/main/config/application.properties)
- [config/application-devkit.properties](https://github.com/bloxbean/yaci-store/blob/main/config/application-devkit.properties)

---

## R-004: Backing store is H2 in-memory

**Decision**: `SPRING_DATASOURCE_URL=jdbc:h2:mem:mydb`. No Postgres sidecar. No persistent volume.

**Rationale**: The feature ships zero downstream consumers of yaci-store (per spec — "pure coverage"). H2 in-memory is fully embedded, requires no extra container, has no operational surface, and gets reset on every `just up`. This is the smallest possible surface to add to the testnet.

**Alternatives considered**:
- File-backed H2 (`jdbc:h2:file:~/storedb`): persistence is irrelevant if nothing queries the store. Rejected.
- Postgres sidecar (mirroring upstream `docker/compose/yaci-store.yml`): introduces a second service, a volume, and DB tuning. Rejected — yaci-store's job here is protocol coverage, not query workload.

**Sources**:
- [docker/compose/yaci-store-monolith.yml](https://github.com/bloxbean/yaci-store/blob/main/docker/compose/yaci-store-monolith.yml) — upstream H2-only example.

---

## R-005: Relay choice and dependency wiring

**Decision**: yaci-store connects to `relay1.example:3001`. Compose `depends_on` includes `configurator` (genesis files must exist before yaci-store starts) and `relay1` (must be reachable on N2N). No other dependencies.

**Rationale**: `relay1` is already the N2N target for the existing `oura` service, so the connection profile is exercised. We deliberately **co-locate** yaci-store on the same relay as oura rather than spreading across `relay2` because: (a) `relay2` is currently quieter (only nodes peer with it), and putting both N2N consumers on the same relay creates more load on a single mini-protocol surface — which is what we want fault injection to stress; (b) it keeps the topology readable.

**Alternatives considered**:
- `relay2`: quieter, less mini-protocol multiplexing pressure. Rejected — we want the busier relay to surface contention findings.
- Both relays (HA pattern): unnecessary for a coverage consumer, and yaci-store has no built-in failover between relays. Rejected.

---

## R-006: Shutdown behaviour and harness implications

**Decision**: Inherit Spring Boot's default SIGTERM handling — graceful shutdown, exit code 0. No `stop_signal`, no `stop_grace_period` override in compose initially. If the first Antithesis run shows non-zero exit codes attributable to yaci-store, this decision is revisited (likely by adding `stop_grace_period: 30s` and possibly `stop_signal: SIGINT`).

**Rationale**: `bloxbean/yaci-store`'s Dockerfile uses `ENTRYPOINT ["java","-jar","yaci-store.jar"]`. Spring Boot translates SIGTERM to a graceful shutdown via the `SpringApplication.exit` path; the JVM exits 0 on success. Unlike Ogmios/Kupo (#49), there is no Haskell GHC SIGTERM-handling gap here.

**Alternatives considered**:
- Pre-emptively configure `stop_signal: SIGINT` on the assumption JVM behaviour might differ: speculative. Rejected per [Verify before claiming](memory) — we measure first.

**Sources**:
- [bloxbean/yaci-store Dockerfile](https://github.com/bloxbean/yaci-store/blob/main/Dockerfile)
- [Issue #49](https://github.com/cardano-foundation/cardano-node-antithesis/issues/49) (for contrast — Ogmios/Kupo SIGTERM regression)

---

## R-007: Healthcheck and depends_on conditions

**Decision**: Add a Docker healthcheck calling Spring Actuator's `/actuator/health` on the container's exposed port. Other services do **not** wait on yaci-store (`depends_on` from yaci-store goes one way: yaci-store → configurator + relay1). yaci-store's own `depends_on` uses `service_completed_successfully` for `configurator` and `service_started` for `relay1`, matching the existing oura/tx-generator pattern.

**Rationale**: The healthcheck is for operator visibility (`docker compose ps` shows `healthy`) and SC-003 verification, not gating other services — yaci-store is a leaf in the dependency graph.

**Alternatives considered**:
- Skip the healthcheck: cheaper, but loses the SC-003 verification path. Rejected.
- Use `condition: service_healthy` for upstream consumers: no upstream consumers exist. Moot.

---

## R-008: Verification path under Antithesis (findings_new ≤ baseline)

**Decision**: Capture the current `main` Antithesis 1h baseline finding count, run a 1h Antithesis duration on `004-yaci-store`, compare. Any new finding must be (a) attributable to yaci-store by container name in the report, AND (b) either filed as a follow-up issue or whitelisted in the harness with a reason. The PR does not merge until findings_new ≤ baseline.

**Rationale**: This is the constitution's hard gate (line 39-40). It applies to any PR that changes the testnet compose.

**Process**:
1. Read `gh workflow run cardano-node.yaml --ref main` output for the current baseline (most recent green 1h run on main).
2. After landing the implementation commit on `004-yaci-store` and getting `publish-images` green, dispatch `gh workflow run cardano-node.yaml --ref 004-yaci-store`.
3. Compare findings via Antithesis Logs Explorer + the `antithesis-triage` skill. File or whitelist as needed.

**Sources**:
- Constitution §IV "Hard gate" (line 39-40)
- `antithesis-triage` skill
