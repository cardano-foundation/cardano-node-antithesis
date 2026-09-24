# Plan — Issue 241

## Technical context (observations at base 6711c4ab, 2026-09-08)

- Six maintained image expressions omit `created`:
  `components/{adversary,asteria-game,asteria-stub,sidecar,tracer-sidecar,tx-generator}/nix/docker-image.nix`.
  No `created` string occurs in any component `nix/` file.
- Each component is an independent flake. `version` derives from
  `self.dirtyShortRev or self.shortRev [or "dev"]`, inconsistently.
- `scripts/push-cardano_node_master_images.sh` resolves Compose tags to
  commits, skips entries whose short and full commit tags both exist in the
  registry (`docker manifest inspect`), skips digest-only pins, and otherwise
  rebuilds via `nix build .#docker-image` + `docker load` + tag + push.
- `testnets/cardano_node_master/docker-compose.yaml` (and siblings) pin
  maintained images by short tag; some third-party entries are digest-only.
- Local CI is `nix develop --quiet -c just ci` (workflows, shell, format,
  two focused suites). It is GREEN at the base. `/gate.sh` is ignored.
- The config image is built by MOOG/Docker, not by these expressions; its
  date (Aug 25 on the Sep-08 run) is a separate artifact with a separate
  owner. Evidence: all 24 completed master-suite runs Aug 16–21 report the
  same freshness failure, so this is not an 11.1.0 regression.

## Timestamp-policy decision (implementation policy, ticket-owner ruling)

Adopt SOURCE-TIME:

- OCI `created` = the source revision time of the exact flake source that
  produced the image (flake `self.lastModifiedDate`, reformatted to
  RFC 3339 UTC). Rationale: reproducible (identical sources yield identical
  bytes and digests), honest (an old source reports an old date; a rebuild
  cannot fake freshness), and cache-coherent (reuse keeps the original
  source date instead of minting a false new one).
- Packaging wall-clock time is NEVER an input to the image closure. Rejected
  alternative: `builtins.currentTime` or build-time stamping breaks
  reproducibility (same source, different digests per build), lets a bare
  rebuild masquerade as a new revision, and poisons the Nix store cache.
- Freshness for Antithesis is therefore earned by a new source revision
  (new commit → new source time → new digest → new tag), never by
  rebuilding. The operator procedure documents exactly this.
- Dirty worktrees build with HEAD's source time but carry an explicit
  `-dirty` marker in revision/version labels; publication requires a clean
  checkout at the resolved commit (the publisher already checks out the
  exact commit).

## Architecture

- New shared helper `components/image-meta/meta.nix` (pure Nix, no I/O),
  consumed as a `path:../image-meta` flake input (`flake = false`) following
  the existing `composer-sdk` precedent. It maps flake `self` to
  `{ created, revision, version, labels }`. One definition serves all six
  components so the policy cannot drift per component.
- Each `nix/docker-image.nix` accepts the helper's record and sets
  `created` plus OCI `config.Labels` (`org.opencontainers.image.created`,
  `.revision`, `.source`, `.version`). Each component `flake.nix` wires the
  new input with no other behavior change.
- New `scripts/check-image-provenance.sh`: given a built image tarball (and
  expected revision/date arguments), it extracts the OCI config from the
  tarball (no daemon), rejects epoch/default `created`, rejects missing or
  mismatched labels, and prints the observed identity on success.
- New `tests/test-image-provenance.sh`: focused proof in the repository's
  existing shell-test idiom (temporary fixtures outside the tracked tree,
  positive control plus seeded-defect negative controls, mutant-style
  self-application checks). It covers the verifier's reject/accept classes
  and the publisher's sanitize/skip predicate with a fake `docker` shim
  (no daemon, no network, no registry mutation).
- `justfile`: new `check-image-provenance` recipe invoked by `ci` (local)
  and by `publish-images.yaml` (hosted, after the build gate). No daemon in
  the check leg; daemon-dependent publication stays in its existing job.
- Docs: policy + operator procedure in `docs/` (policy page plus component
  wiring notes), including the residual Antithesis boundary (MOOG config
  image, intentionally old pins, need for a consuming run).

## Slice

Single bisect-safe slice S1 (one logical change, mechanical fan-out):

1. Helper + policy doc; all six image expressions + flake wirings.
2. Verifier script + focused test suite; `just ci` and workflow wiring.
3. RED/GREEN artifact evidence on representative images (cheapest first:
   asteria-stub, tx-generator; at least one Haskell-built image only if the
   remaining execution budget allows, otherwise documented with the exact
   unrun command as residual).
4. Warm-cache rebuild receipt; publisher skip-predicate receipts.

## Live boundaries

- OCI inspection is daemon-free (tarball reads). Publication (`docker push`,
  `docker manifest inspect` against the registry) and the Antithesis
  freshness verdict require network/registry/paid runs and are NOT part of
  local acceptance; they are documented residuals with exact commands.
- No full uncached Cardano Node build is used to test this packaging task.
