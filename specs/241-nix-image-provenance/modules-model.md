# Modules model — Issue 241

## New: image-meta helper

- `components/image-meta/meta.nix` — sole owner of the timestamp/provenance
  computation. Pure function over flake `self`. No I/O, no network, no
  wall-clock reads. Dependency direction: every component image expression
  depends on it; it depends on nothing in the repository.

## Changed: component image expressions (6)

- `components/{adversary,asteria-game,asteria-stub,sidecar,tracer-sidecar,tx-generator}/nix/docker-image.nix`
  — accept the helper record; set OCI `created` and `config.Labels`.
  No entrypoint, command, closure, or tag-scheme change.

## Changed: component flakes (6)

- `components/{adversary,asteria-game,asteria-stub,sidecar,tracer-sidecar,tx-generator}/flake.nix`
  — add the `path:../image-meta` input (`flake = false`), evaluate the
  helper over `self`, pass the record into `nix/docker-image.nix`.
  No input bumps, no package changes.

## New: verifier and proof

- `scripts/check-image-provenance.sh` — reads one built image tarball plus
  expected revision/date; verdict on stdout/stderr with exit status.
  Depends on the OCI tarball layout only, never on component internals.
- `tests/test-image-provenance.sh` — focused proof for the verifier classes
  and the publisher skip predicate (fake-`docker` shim). Depends on the
  verifier and on temporary fixtures it creates; never on the registry.

## Changed: gates

- `justfile` — new `check-image-provenance` recipe; `ci` depends on it.
- `.github/workflows/publish-images.yaml` — invoke the provenance check
  after the build gate (daemon-free leg stays daemon-free).

## Changed: docs

- Policy + operator procedure under `docs/` (exact pages chosen by the
  worker inside `docs/components/` or a new `docs/images/` leaf; no
  restructuring of unrelated pages).

## Promotion

No abstraction is promoted to a stable upstream owner: the helper is
repository-local by design (it encodes this repository's flake-input
precedent). If a second repository needs the same rule, promotion is a
separate ticket.
