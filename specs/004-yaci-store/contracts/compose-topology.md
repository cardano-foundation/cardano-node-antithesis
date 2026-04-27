# Contract: compose-topology (yaci-store)

**Feature**: 004-yaci-store
**Date**: 2026-04-27
**Authoritative spec for the docker-compose service stanza this feature lands.**

## yaci-store service stanza (forward-looking)

This is the YAML the implementation commit lands in `testnets/cardano_node_master/docker-compose.yaml`, immediately after the `oura` service for visual proximity (both are N2N consumers).

```yaml
yaci-store:
  image: docker.io/bloxbean/yaci-store@sha256:<DIGEST_FROM_DOCKER_HUB_2.0.0>
  container_name: yaci-store
  hostname: yaci-store.example
  environment:
    STORE_CARDANO_HOST: relay1.example
    STORE_CARDANO_PORT: 3001
    STORE_CARDANO_PROTOCOL_MAGIC: 42
    STORE_CARDANO_BYRON_GENESIS_FILE: /configs/configs/byron-genesis.json
    STORE_CARDANO_SHELLEY_GENESIS_FILE: /configs/configs/shelley-genesis.json
    STORE_CARDANO_ALONZO_GENESIS_FILE: /configs/configs/alonzo-genesis.json
    STORE_CARDANO_CONWAY_GENESIS_FILE: /configs/configs/conway-genesis.json
    SPRING_DATASOURCE_URL: jdbc:h2:mem:mydb
  volumes:
    - p1-configs:/configs:ro
  healthcheck:
    test: ["CMD-SHELL", "wget --quiet --tries=1 --spider http://localhost:8080/actuator/health || exit 1"]
    interval: 10s
    timeout: 3s
    retries: 12
    start_period: 30s
  restart: always
  depends_on:
    configurator:
      condition: service_completed_successfully
    relay1:
      condition: service_started
```

## Bind-mount rule (Antithesis constraint)

This service introduces **no** bind mount via host path. The single mount is the existing named volume `p1-configs`, which is already declared in `volumes:` at the bottom of the compose file. No relative path leaves the compose directory. ✅ Antithesis-compatible.

## Image digest pin

The `sha256:` digest is computed at implementation time:

```bash
docker pull docker.io/bloxbean/yaci-store:2.0.0
docker inspect --format='{{index .RepoDigests 0}}' docker.io/bloxbean/yaci-store:2.0.0
```

The output is something like `bloxbean/yaci-store@sha256:abcd…` and that is the exact value substituted into the image field. The tag side (`:2.0.0`) is dropped per the existing pattern (commit f3edbe0 — "drop tag side from digest-pinned images").

## Genesis filename confirmation

The placeholders `byron-genesis.json` etc. assume the configurator output. Implementation task **must** verify by:

```bash
docker compose -f testnets/cardano_node_master/docker-compose.yaml up -d configurator
docker run --rm -v cardano-node-master_p1-configs:/configs:ro alpine ls /configs/configs/
```

If filenames differ, the env vars are updated in the same commit. The spec/plan/tasks docs are amended to match.

## Startup ordering

Cold `docker compose up -d` sequence:
1. `configurator` runs to completion (creates `p1-configs/configs/`).
2. `relay1` starts (no dependency on yaci-store).
3. `yaci-store` starts after `configurator` exits 0 and `relay1` is started.
4. `yaci-store` JVM boots, reads genesis, opens N2N to `relay1.example:3001`, begins chain-sync.
5. Healthcheck passes within `start_period` (30s) + a few intervals — well inside SC-003's 1-minute target on a devnet machine.

## Teardown

`just down` performs `docker compose down --volumes --remove-orphans`. yaci-store stops via SIGTERM. Spring Boot graceful shutdown logs to stdout, exits 0. The H2 in-memory database evaporates with the container. No dangling volumes or networks (the only volume yaci-store touches is the existing `p1-configs`, which is removed by `--volumes`).

## Versioning rule

When upstream `bloxbean/yaci-store` releases a new stable version we want to adopt, the bump is a one-line PR:
1. `docker pull docker.io/bloxbean/yaci-store:<NEW>`
2. `docker inspect --format='{{index .RepoDigests 0}}'` to get the new digest.
3. Replace the digest in the compose stanza.
4. Re-run the constitution's verify-before-merge sequence (`publish-images` green → 1h Antithesis → finding diff).

No code changes in this repo accompany an upstream version bump.
