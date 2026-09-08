# Functions model — Issue 241

Only new/changed signatures. No bodies, no algorithms.

## `components/image-meta/meta.nix`

- `mkImageMeta({ self, sourceUrl }) -> { created, revision, version, labels }`
  - `self`: flake self (`lastModifiedDate`, `rev`/`dirtyRev`/`shortRev`,
    `lastModified` as fallback).
  - `sourceUrl`: string, repository URL.
  - Returns: `created`: RFC 3339 UTC string; `revision`: string;
    `version`: string; `labels`: attrset of the four OCI label bindings.
  - Constraint: pure; no `builtins.currentTime`, no I/O, no network.

## `scripts/check-image-provenance.sh`

- `check-image-provenance.sh --image <tarball> --expected-revision <rev> --expected-created <rfc3339> --expected-source <url> --expected-version <ver>`
  - `tarball`: path to a `dockerTools.buildImage` output tarball.
  - `expected-revision`: full SHA with optional `-dirty`. No unpublished
    marker exists: sources without revision metadata fail closed before
    verification (helper throws), so the verifier never certifies them.
  - `expected-created`: RFC 3339 UTC timestamp.
  - `expected-source`: repository URL compared for equality against
    `org.opencontainers.image.source`.
  - `expected-version`: image tag compared for equality against
    `org.opencontainers.image.version`.
  - Effect: read-only inspection; no daemon, no network, no mutation.
  - Success: exit 0 plus one canonical identity line
    (`created=… revision=… config_digest=… tarball_sha256=…`).
  - Failure: exit 1 with `FAIL …` diagnostics for epoch/default `created`,
    missing labels, or any mismatch; empty/unreadable input fails.

## `tests/test-image-provenance.sh`

- `test-image-provenance.sh` (no arguments)
  - Effect: builds temporary fixtures under a private temp dir (never the
    tracked tree), runs the verifier's accept/reject classes and the
    publisher skip-predicate controls, prints one `PASS …` line per class.
  - Success: exit 0 with every class passing and every seeded defect
    rejected for the named reason.
  - Failure: exit 1 naming the first undefeated class.

## `justfile`

- `just check-image-provenance` — runs the focused proof plus the
  representative tarball inspection. `just ci` gains it as a dependency.
