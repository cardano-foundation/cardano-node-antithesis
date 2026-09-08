# Data model — Issue 241

## OCI artifact fields (observed in the emitted tarball config)

- `created`: RFC 3339 UTC timestamp. Legitimate values equal the source
  revision time. The value `1970-01-01T00:00:01Z` (nixpkgs
  `dockerTools.buildImage` default) is the epoch defect.
- `config.Labels."org.opencontainers.image.created"`: mirrors `created`.
- `config.Labels."org.opencontainers.image.revision"`: full source commit
  SHA, with `-dirty` suffix when the producing tree was dirty.
- `config.Labels."org.opencontainers.image.source"`: repository URL
  (`https://github.com/cardano-foundation/cardano-node-antithesis`).
- `config.Labels."org.opencontainers.image.version"`: short revision tag
  (the existing `version` semantics, `-dirty` marked when dirty).

## Helper record (pure, per build)

- `created`: RFC 3339 UTC derived from flake `self.lastModifiedDate`.
- `revision`: `self.rev or self.dirtyRev` plus dirtiness marker, or `"dev"`
  only when no revision metadata exists (never published).
- `version`: existing short-rev semantics, `-dirty` marked when dirty.
- `labels`: the four OCI label bindings above.

Validation: `created` matches `^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$`
and is not the epoch default; `revision` matches `^[0-9a-f]{40}(-dirty)?$`
or the explicit unpublished marker.

## Verifier I/O

- Inputs: image tarball path, expected revision, expected created date.
- Success output: one canonical line with observed `created`, revision,
  config digest, and tarball SHA-256. Failure: diagnostics on stderr,
  exit non-zero. An empty tarball set or unreadable config is a failure,
  never a pass.

## Publisher entries (existing behavior, preserved)

- `name tag` pairs parsed from Compose files after digest stripping;
  digest-only pins carry no tag and are skipped with a report line.
- Skip predicate: both `name:tag` and `name:commit` manifests exist →
  skip with a report line; otherwise rebuild at the exact commit.

## Invariants

- INV-241-CREATED-TRUTH (ADVISORY): emitted OCI `created` equals the source
  revision time. Failure: epoch/default date. Success: exact documented
  source date, observed in the tarball config.
- INV-241-PROVENANCE (ADVISORY): OCI labels carry revision/source/version
  matching the producing inputs. Failure: missing label or mismatch.
  Success: all labels present and equal to inputs.
- INV-241-REPRODUCIBLE (ADVISORY): identical sources yield identical image
  bytes. Failure: wall-clock or other unpinned input inside the closure.
  Success: consecutive builds share the store path.
- INV-241-VERIFY-ARTIFACT (ADVISORY): acceptance rests on tarball-config
  inspection. Failure: source-only grep standing as proof. Success: every
  verdict cites tarball SHA-256 plus config digest.
- INV-241-CACHE-HONEST (ADVISORY): rebuilds of identical sources report
  reuse. Failure: a repeated build minting or claiming a new identity.
  Success: same store path reported as reused with the original date.
- INV-241-PUBLISH-SKIP (ADVISORY): immutable published identities are never
  overwritten; new sources get new tags the consumer selects. Failure:
  retagging old bytes or skipping a corrected tag. Success: skip lines for
  published tags, new tags for new sources, consumer file updated.
