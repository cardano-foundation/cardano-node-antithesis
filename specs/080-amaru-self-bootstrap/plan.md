# Implementation Plan: Amaru relays self-bootstrap

**Branch**: `080-amaru-self-bootstrap` | **Date**: 2026-05-02 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/080-amaru-self-bootstrap/spec.md`

## Summary

Remove the standalone `bootstrap-producer` service from
`testnets/cardano_amaru_epoch3600/docker-compose.yaml`. Move its retry loop
+ `/bin/bootstrap-producer` invocation into each `amaru-relay-N`
container's entrypoint, paired with that relay's upstream cardano-node
producer (relay-1 ↔ p1, relay-2 ↔ p2). Each relay now does its own
bootstrap to producer state, writes its `/amaru-startup` marker, then
`exec /bin/amaru run`. Drop the shared `amaru-bundle` and
`bootstrap-state` volumes — per-relay scratch lives on the existing
`a1-state` / `a2-state` volumes. Net effect: Antithesis "setup complete"
fires within seconds (every container is `service_started` immediately);
bootstrap work happens during the test phase where the budget is hours,
not 6 minutes.

The amaru-bootstrap-producer image (`ghcr.io/lambdasistemi/amaru-bootstrap-producer:pr-32-…`)
is reused unchanged — it already ships `/bin/sh`, `/bin/bootstrap-producer`,
and `/bin/amaru`. No new components, no new images, no upstream changes
required.

## Technical Context

**Language/Version**: Bourne shell (`/bin/sh`) for the entrypoint; YAML for compose.
**Primary Dependencies**: `podman-compose` / `docker compose` ≥ 2.36; the existing `amaru-bootstrap-producer` image (ships `/bin/sh`, `/bin/bootstrap-producer`, `/bin/amaru`).
**Storage**: Per-relay named volumes `a1-state`, `a2-state` (existing); `pN-state` mounted read-only from the upstream cardano-node producers.
**Testing**: `just smoke-test` (or local `docker compose up`) for the testnet; Antithesis 1h dispatch on the branch for the binding gate.
**Target Platform**: Linux, podman-in-VM under Antithesis sandbox; Docker on local dev.
**Project Type**: Compose topology refactor inside a single testnet directory.
**Performance Goals**: `compose up` to `service_started`-on-everything in <90 s on the Antithesis VM (4 vCPU / 16 GB).
**Constraints**: Antithesis setup deadline = 6 min hard. Per-relay disk for the bootstrap scratch copy must stay within the testnet's existing storage envelope (~hundreds of MB).
**Scale/Scope**: 1 testnet (`cardano_amaru_epoch3600`), 2 amaru-relay services, 1 compose file edited, 0 new components.

## Constitution Check

*Re-evaluated post-design — all gates pass.*

| Principle | Applies? | Status | Notes |
|-----------|----------|--------|-------|
| I. Composer-first workload | No | n/a | This is a compose topology change; no composer commands added or modified. |
| II. SDK instrumentation | No | n/a | No new SDK code paths. |
| III. Short-running commands | No | n/a | Not a composer command. |
| IV. Duration-robust | Yes | Pass | Bootstrap moves into the test phase; works the same at 1h and 3h. Longer durations only give more headroom. |
| V. Realistic workload | No | n/a | No workload generation changes. |
| VI. Bisect-safe commits | Yes | Pass | Single self-contained compose-file diff; no intermediate state. `just smoke-test` runs both before and after. |
| VII. Image tag hygiene | Yes | Pass | No `ghcr.io/cardano-foundation/cardano-node-antithesis/<name>` images change. The upstream `lambdasistemi/amaru-bootstrap-producer` tag is unchanged (same digest, same image, just used differently). |
| Composer smoke-tests run locally | No | n/a | No composer changes. |
| Custom testnet boundaries | Yes | Pass | No mainnet dependencies introduced; bootstrap reads from local `pN-state`. |
| Minimal sidecar image | Yes | Pass | Sidecar untouched. |
| Hard gate: `findings_new <= baseline` | Yes | Will run | Plan includes an Antithesis 1h dispatch on `080-amaru-self-bootstrap` before merge; baseline is current `feat/amaru-bootstrap-producer-cluster` (which today fails setup, so the binding comparison is against `main`'s last green Amaru run on this testnet). |

**Note on scope of "Image tag hygiene"**: the constitution mandates a tag bump *when a PR modifies a component's source*. This PR does not modify any `components/<name>/` directory, so no tag bump is required.

## Project Structure

### Documentation (this feature)

```text
specs/080-amaru-self-bootstrap/
├── spec.md              # Feature spec (already written)
├── plan.md              # This file
├── research.md          # Phase 0 — open questions resolved before edits
├── data-model.md        # Phase 1 — volume/marker contract reference
├── quickstart.md        # Phase 1 — how to validate locally + on Antithesis
└── checklists/
    └── requirements.md  # Spec quality checklist (already written)
```

`tasks.md` is **out of scope for `/speckit.plan`** — created by `/speckit.tasks` next.

### Source Code (repository root)

Only one file is edited; no new directories. The plan-relevant tree:

```text
cardano-node-antithesis/
├── testnets/
│   └── cardano_amaru_epoch3600/
│       ├── docker-compose.yaml      ← THE edit (remove bootstrap-producer
│       │                              service + amaru-bundle/bootstrap-state
│       │                              volumes; rewrite amaru-relay-N
│       │                              command and depends_on; mount pN-state
│       │                              into amaru-relay-N as /live:ro)
│       ├── amaru-runtime/           (unchanged)
│       ├── relay-topology.json      (unchanged)
│       ├── testnet.yaml             (unchanged)
│       └── tracer-config.yaml       (unchanged)
└── components/                      (untouched)
```

**Structure Decision**: Single-file compose-topology edit. The implementation surface is one YAML file. Bash inside that YAML inherits the shape of the existing `bootstrap-producer.command:` block — same retry semantics, same exit-code handling, just hoisted into each relay's entrypoint.

## Phase 0 — Research / open questions

Captured separately in [`research.md`](./research.md). Summary of questions
resolved before writing the diff:

1. Do `pN-state` volumes need to be mounted read-only into the relay
   *and* the producer simultaneously? — Yes; the existing bootstrap-producer
   already mounts `p1-state:/live:ro` while p1 has `p1-state:/state` (rw).
   Adding a second read-only mount of the same volume into amaru-relay-N is
   a no-op for the producer.
2. Does the relay container need both `pN-state:/live:ro` AND
   `pN-configs:/cardano/config:ro`? — Yes; `/bin/bootstrap-producer` reads
   the config dir for genesis/protocol params alongside the state.
3. Is concurrent read of one `pN-state` volume by both `pN` (rw) and
   `amaru-relay-N` (ro) safe under the bootstrap-producer's
   "snapshot-then-validate" approach (which is why it retries on codes
   1/2/5/7)? — Yes, that's exactly the case the existing retry-loop covers;
   we inherit it unchanged.
4. Does `restart: always` on `amaru-relay-N` interact badly with the
   in-entrypoint bootstrap loop on container restart? — No; the
   `bundle_complete` short-circuit at the top of the loop makes a restart
   idempotent (already-bootstrapped relays skip straight to `amaru run`).
5. Do we need a final `bundle_complete`-equivalent for the relay's view
   (`/srv/amaru/ledger.testnet_42.db` etc.) or can we drop the
   `testnet_42/` subdir indirection? — We drop the indirection. The
   bootstrap-producer's "commit" used to be: write to
   `/srv/amaru/testnet_42/{ledger,chain,nonces}`; relays then `cp -rL
   /bundle/testnet_42/. /srv/amaru/`. Now the relay writes
   `/bin/bootstrap-producer …` output into `/srv/amaru/testnet_42/` and
   keeps the same final `cp -rL` — fewer moving parts than refactoring
   the on-disk layout.

## Phase 1 — Design artifacts

### `data-model.md`

The "data model" here is volumes + a marker file, all inherited from the
existing testnet. See [`data-model.md`](./data-model.md) for the
volume-by-volume mapping (which volumes go away, which are reused, which
mount points change).

### `contracts/`

**Skipped** — this feature has no external API/CLI/grammar surface.
Internal contracts (the existing `/bin/bootstrap-producer` exit-code
contract, the `/amaru-startup/<relay>.started` marker contract, the
sidecar's `AMARU_STARTUP_*` env contract) are preserved verbatim and
documented in `data-model.md`.

### `quickstart.md`

How to validate the change locally and on Antithesis. See
[`quickstart.md`](./quickstart.md).

### Agent context update

This repo's agent context is `CLAUDE.md`. No update needed — the change
is a single compose-file edit confined to one testnet directory and
introduces no new technologies, languages, or conventions.

## Complexity Tracking

No constitution violations to justify.
