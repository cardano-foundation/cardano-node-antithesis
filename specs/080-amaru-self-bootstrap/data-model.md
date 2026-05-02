# Phase 1 Data Model: Amaru relays self-bootstrap

The "data model" for this feature is the compose volume topology and the
file-level contracts between the relay entrypoint, the
`/bin/bootstrap-producer` binary, and the sidecar.

## Volume changes

| Volume | Before | After | Notes |
|--------|--------|-------|-------|
| `p1-state` | rw on `p1`; ro on `bootstrap-producer` (`/live`) | rw on `p1`; **ro on `amaru-relay-1` (`/live`)** | New mount target; producer is unaware. |
| `p2-state` | rw on `p2` | rw on `p2`; **ro on `amaru-relay-2` (`/live`)** | New mount target. |
| `p3-state` | rw on `p3` | rw on `p3` | Unchanged. |
| `p1-configs` | rw on `configurator`; ro on `p1` and `bootstrap-producer` (`/cardano/config`) | rw on `configurator`; ro on `p1` and **ro on `amaru-relay-1`** | New mount target. |
| `p2-configs` | rw on `configurator`; ro on `p2` | rw on `configurator`; ro on `p2`; **ro on `amaru-relay-2`** | New mount target. |
| `bootstrap-state` | rw on `bootstrap-producer` (`/cardano/state`) | **DELETED** | Per-relay scratch lives on `aN-state` instead. |
| `amaru-bundle` | rw on `bootstrap-producer` (`/srv/amaru`); ro on `amaru-relay-{1,2}` (`/bundle`) | **DELETED** | Inter-container handoff no longer needed. |
| `a1-state` | rw on `amaru-relay-1` (`/srv/amaru`) | rw on `amaru-relay-1` (`/srv/amaru`) — now also holds the `testnet_42/` scratch subdir | Same volume, expanded use. |
| `a2-state` | rw on `amaru-relay-2` (`/srv/amaru`) | rw on `amaru-relay-2` (`/srv/amaru`) — now also holds the `testnet_42/` scratch subdir | Same volume, expanded use. |
| `amaru-startup` | rw on `amaru-relay-{1,2}` (`/startup`); ro on `sidecar` (`/amaru-startup`) | unchanged | Sidecar contract preserved. |

## Service topology

| Service | Before | After |
|---------|--------|-------|
| `bootstrap-producer` | Defined; one-shot; `restart: "no"`; `depends_on: p1:service_started` | **DELETED** |
| `amaru-relay-1` | `depends_on: bootstrap-producer:service_completed_successfully` + image-via-anchor; entrypoint waits on `/bundle/testnet_42/...` then `cp -rL` then `amaru run --peer-address p1.example:3001` | `depends_on: p1:service_started`; entrypoint runs the bootstrap-loop against `/live` (= p1-state) → writes `/srv/amaru/testnet_42/...` → `cp -rL` to `/srv/amaru/` → marker → `exec amaru run --peer-address p1.example:3001` |
| `amaru-relay-2` | same as relay-1 but paired with p2 | same as relay-1 but paired with p2 |
| `sidecar` | `AMARU_STARTUP_REQUIRED=true`, `AMARU_STARTUP_DIR=/amaru-startup`, `AMARU_RELAYS="amaru-relay-1 amaru-relay-2"` | unchanged |

## File-level contracts (preserved verbatim)

### `/bin/bootstrap-producer` exit codes

Inherited from the current image; the relay entrypoint MUST treat them
identically:

| Code | Meaning | Action in relay entrypoint |
|------|---------|----------------------------|
| 0 | Success — `wrote …` line in attempt log; final dir committed | Break the bootstrap loop; proceed to marker + `exec amaru run`. |
| 1, 2, 5, 7 | Transient (snapshot copied before enough immutable history; Antithesis fault during read; partial files) | Sleep `AMARU_BOOTSTRAP_RETRY_SECONDS`; refresh snapshot; retry. |
| Any other non-zero | Hard failure | `tail -n 50` of attempt log to stderr; `exit "$rc"` (relay container exits non-zero — surfaces to Antithesis as a container failure). |

### Marker file contract (consumed by sidecar)

- Path: `/startup/<container-name>.started` inside the relay
  (`/amaru-startup/<container-name>.started` from the sidecar's
  perspective via the shared `amaru-startup` volume).
- Contents: hostname, one line. (Existing pattern;
  `printf '%s\n' "$HOSTNAME"`.)
- Write timing: AFTER `bundle_complete` is true (i.e. after bootstrap
  succeeded and the `/srv/amaru/{ledger,chain,nonces}` layout is in
  place), BEFORE `exec /bin/amaru run`.
- Idempotency: if marker already exists from a prior container instance
  (volume survives restart), overwriting is a no-op.

### Environment variables (preserved)

| Var | Default | Read by | Notes |
|-----|---------|---------|-------|
| `AMARU_BOOTSTRAP_RETRY_SECONDS` | `5` (was `20` in the bootstrap-producer's `${VAR:-20}` default; the per-service override sets it to `5`) | Relay entrypoint | Sleep between retries. |
| `AMARU_LOG`, `AMARU_TRACE`, `AMARU_COLOR` | from `x-amaru` anchor | `amaru run` (later in the same container) | Unchanged. |
| `AMARU_NETWORK`, `AMARU_CLUSTER_READY_DEADLINE_SECONDS`, `AMARU_WAIT_DEADLINE_SECONDS`, `AMARU_POLL_INTERVAL_SECONDS` | from current `bootstrap-producer.environment` | `/bin/bootstrap-producer` | Move from `bootstrap-producer.environment` into each `amaru-relay-N.environment` (or into the `x-amaru` anchor). |

## Producer/relay pairing

Preserved exactly as today:

| Relay | Producer state read | `--peer-address` used by `amaru run` |
|-------|---------------------|--------------------------------------|
| `amaru-relay-1` | `p1-state` (via `/live:ro`) + `p1-configs` (via `/cardano/config:ro`) | `p1.example:3001` |
| `amaru-relay-2` | `p2-state` (via `/live:ro`) + `p2-configs` (via `/cardano/config:ro`) | `p2.example:3001` |
