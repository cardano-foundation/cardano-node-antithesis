# Data Model: yaci-store as Antithesis N2N consumer

**Feature**: 004-yaci-store
**Date**: 2026-04-27

This feature has no application data model — yaci-store's internal indexed schema is upstream-defined and irrelevant to the testnet (we never query it). What matters is the **container's external interface**: the configuration surface, the volumes, and the dependency graph.

## Service entity: `yaci-store`

| Attribute | Value | Source |
|-----------|-------|--------|
| Service name | `yaci-store` | this feature |
| Container name | `yaci-store` | this feature |
| Hostname | `yaci-store.example` | matches existing testnet convention |
| Image | `docker.io/bloxbean/yaci-store@sha256:<digest>` | R-001, R-002 |
| Restart policy | `always` | matches all other long-running services |

## Configuration entity: env-var surface

All keys come from `store.cardano.*` and `spring.datasource.*` in upstream `application.properties`. Spring Boot maps them by uppercasing and converting `.`/`-` to `_`.

| Env var | Value | Purpose | Source |
|---------|-------|---------|--------|
| `STORE_CARDANO_HOST` | `relay1.example` | N2N target host | R-005 |
| `STORE_CARDANO_PORT` | `3001` | N2N target port | matches relay listening port |
| `STORE_CARDANO_PROTOCOL_MAGIC` | `42` | testnet protocol magic | matches `oura-daemon.toml`, configurator |
| `STORE_CARDANO_BYRON_GENESIS_FILE` | `/configs/configs/byron-genesis.json` | byron genesis path | R-003 (path confirmed at impl) |
| `STORE_CARDANO_SHELLEY_GENESIS_FILE` | `/configs/configs/shelley-genesis.json` | shelley genesis path | R-003 |
| `STORE_CARDANO_ALONZO_GENESIS_FILE` | `/configs/configs/alonzo-genesis.json` | alonzo genesis path | R-003 |
| `STORE_CARDANO_CONWAY_GENESIS_FILE` | `/configs/configs/conway-genesis.json` | conway genesis path | R-003 |
| `SPRING_DATASOURCE_URL` | `jdbc:h2:mem:mydb` | embedded H2 in-memory | R-004 |

Notes:
- `STORE_CARDANO_DEVKIT_NODE` is **not** set. Setting it routes yaci-store through the yaci-devkit code path which expects yaci-cli, not a real cardano-node N2N relay.
- N2C-related keys (`store.cardano.n2c-*`) are **not** set — yaci-store's role here is N2N read-only.

## Volume entity: shared mounts

| Mount | Source | Target | Mode | Purpose |
|-------|--------|--------|------|---------|
| Genesis files | `p1-configs` (named volume populated by configurator) | `/configs` | `ro` | Read genesis JSON files |

No new volume is created. yaci-store reads genesis from the same volume `p1` reads its own configs from. This works because the configurator emits identical genesis files into `p1-configs`, `p2-configs`, `p3-configs` — picking `p1-configs` is arbitrary; it could be any of the three.

## Dependency graph

```
configurator ─────► yaci-store
                    │
relay1 ─────────────┘
```

Compose `depends_on`:
```yaml
depends_on:
  configurator:
    condition: service_completed_successfully
  relay1:
    condition: service_started
```

No service depends on yaci-store. It is a leaf in the testnet dependency graph.

## Healthcheck entity

```yaml
healthcheck:
  test: ["CMD-SHELL", "curl -f http://localhost:8080/actuator/health || exit 1"]
  interval: 10s
  timeout: 3s
  retries: 12
  start_period: 30s
```

The exact command depends on whether the upstream image ships `curl`. If it does not, `wget --quiet --tries=1 --spider http://localhost:8080/actuator/health` is the fallback. Implementation task confirms which is available before locking the recipe.

## State transitions (implicit)

yaci-store is a passive consumer. There are no testnet-driven state transitions on yaci-store the test cares about. The only externally observable state is:

- **Starting** → container running, JVM warming, no chain follow yet.
- **Following tip** → yaci-store has caught up to relay tip and continues following.
- **Reconnecting** → relay1 went down or a partition was injected; yaci-store retries with backoff (Spring Boot default).
- **Shutting down** → SIGTERM received, graceful shutdown in progress, exit imminent.
- **Stopped** → exit code captured by harness.

These map directly to acceptance scenarios in `spec.md` (User Story 1 / 2). They are not modeled formally; they are observable in container logs and via `docker compose ps`.
