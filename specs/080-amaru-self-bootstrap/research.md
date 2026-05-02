# Phase 0 Research: Amaru relays self-bootstrap

All questions resolved before writing the diff. Each entry records the
decision, why, and what was considered instead.

---

## R1. Can `pN-state` be mounted read-only in `amaru-relay-N` while `pN` mounts it read-write?

**Decision**: Yes. Mount `p1-state:/live:ro` in `amaru-relay-1` and
`p2-state:/live:ro` in `amaru-relay-2`. The producers `p1`/`p2` keep
their existing `pN-state:/state` (rw) mounts.

**Rationale**: This is exactly what the current `bootstrap-producer`
service does — it mounts `p1-state:/live:ro` while p1 is writing to the
same volume. Reads are tolerated by the OS layer; the bootstrap-producer
binary expects snapshot-style semantics (not a transactional read) and
its retry-on-codes-1/2/5/7 already covers the "I read a half-written
file" case.

**Alternatives considered**: Snapshotting the producer state to a
sidecar volume first. Rejected because it adds a new service, defeating
the point of the refactor.

---

## R2. Do both producer state AND producer config dirs need to be mounted?

**Decision**: Yes. Each relay needs `pN-state:/live:ro` AND
`pN-configs:/cardano/config:ro`.

**Rationale**: The current `bootstrap-producer.command` invokes
`/bin/bootstrap-producer /cardano/state /cardano/config/configs /srv/amaru testnet_42`.
The second positional arg is the genesis/protocol-params config tree.
Without it, bootstrap-producer cannot construct a valid initial ledger
state.

**Alternatives considered**: Bake configs into the
amaru-bootstrap-producer image. Rejected — the testnet's configurator
generates `pN-configs` per-pool at runtime from `testnet.yaml`; baked
configs would drift.

---

## R3. Concurrent readers on a single `pN-state` volume — safe?

**Decision**: Yes, no mitigation needed beyond the existing retry loop.

**Rationale**: There is exactly one rw writer (the cardano-node producer
`pN`) and one ro reader (`amaru-relay-N`) per volume. Each relay reads
its *own* paired producer's state — no two relays read the same volume.
Half-written reads are caught by `/bin/bootstrap-producer`'s exit
codes 1/2/5/7, which are already in the existing retry whitelist.

**Alternatives considered**:
- Coordinate snapshot via `flock` — rejected; introduces a new sync
  primitive across containers.
- Have one relay snapshot, the other consume — that's exactly what
  bootstrap-producer does today and is the architecture we're undoing.

---

## R4. Does `restart: always` on `amaru-relay-N` interact correctly with the in-entrypoint bootstrap loop?

**Decision**: Yes — keep `restart: always`. The bootstrap loop's
`bundle_complete()` check at the top makes restart idempotent: if a
restarted relay finds its `/srv/amaru/ledger.testnet_42.db/live`,
`/srv/amaru/chain.testnet_42.db`, and `/srv/amaru/nonces.json` already
present (from the previous container's run on the same `aN-state`
volume), the loop short-circuits to `exec amaru run` without redoing
the bootstrap.

**Rationale**: The current `bootstrap-producer` already has this
short-circuit; we hoist the same logic into the relay. The
`a1-state`/`a2-state` volumes survive container restarts.

**Alternatives considered**: `restart: on-failure`. Rejected — Antithesis
fault injection routinely SIGKILLs containers; we want the relay to come
back up and resume.

---

## R5. On-disk layout — keep the `testnet_42/` subdir indirection or flatten?

**Decision**: Keep the `testnet_42/` indirection. The bootstrap-producer
binary writes to `/srv/amaru/testnet_42/{ledger.testnet_42.db,
chain.testnet_42.db, nonces.json}`; the relay's existing entrypoint
already does `cp -rL /bundle/testnet_42/. /srv/amaru/` to flatten before
invoking `amaru run`. Reuse that copy step verbatim, just sourcing from
the local in-container scratch dir instead of the cross-container
`amaru-bundle` volume.

**Rationale**: The smallest, safest diff. Flattening would mean either
patching the bootstrap-producer binary (out of scope, upstream change)
or post-processing (more shell complexity). The `cp -rL` is fast and
proven.

**Alternatives considered**: Patch upstream bootstrap-producer to write
directly to the flat layout. Rejected — out of scope for this
testnet-side change; would couple us to a new image build.

---

## R6. Are there other consumers of `bootstrap-producer` or `amaru-bundle` we'd break?

**Decision**: No. Confirmed by grepping the testnet directory and the
sidecar/composer config:

- `bootstrap-producer` is only referenced by:
  - The service definition itself in
    `testnets/cardano_amaru_epoch3600/docker-compose.yaml`.
  - `amaru-relay-1.depends_on` and `amaru-relay-2.depends_on` (both being
    rewritten by this change).
- `amaru-bundle` volume is only mounted by:
  - `bootstrap-producer` (rw) — being removed.
  - `amaru-relay-1` and `amaru-relay-2` (`/bundle:ro`) — being removed.
- `bootstrap-state` volume is only mounted by `bootstrap-producer` —
  being removed.

The sidecar reads `/amaru-startup/*.started`; that contract is
preserved.

**Rationale**: Deletion is safe.

**Alternatives considered**: Deprecate gradually (leave the volumes,
just don't use them). Rejected — dead volumes waste Antithesis disk and
add noise to the report's container/volume list.

---

## R7. Should the new relay entrypoint be inlined in compose YAML or shipped as a script in the image?

**Decision**: Inline in compose YAML, mirroring the current
`bootstrap-producer.command:` pattern.

**Rationale**: The image is upstream
(`ghcr.io/lambdasistemi/amaru-bootstrap-producer:pr-32-…`). Adding a
script to the image means another upstream change + image rebuild +
tag bump cycle. Inlining keeps the iteration loop entirely in
`testnets/cardano_amaru_epoch3600/docker-compose.yaml`.

**Alternatives considered**:
- Ship the script as a bind-mounted file from the testnet dir — would
  work but adds another file to maintain; the YAML inlining is what the
  testnet already uses (see existing `bootstrap-producer.command`).
- Bake into image — see above; out of scope.
