# Spec — Issue 241: truthful Nix component image timestamps and provenance

## P1 user story

As an Antithesis operator, I build and publish a Nix component image and
observe an explicit, truthful creation date and source identity in the actual
OCI artifact, with verification that the intended artifact reaches the test
configuration.

## Problem

The Nix-built component images record Docker creation date
`1970-01-01T00:00:01Z`: every maintained `dockerTools.buildImage` expression
omits `created`, keeping the reproducible epoch default. Antithesis's "Recent
software version provided" property requires at least one supplied image
created within 72 hours and does not inspect the software revision. The
harness therefore supplies no usable image-age information. Rebuilding or
republishing from cache is not evidence of a fresh source revision, and the
publisher skips images whose short and full commit tags already exist.

## Functional requirements

- REQ-241-01: One explicit timestamp policy is defined and documented for all
  maintained Nix component images. It distinguishes source time, packaging
  time, and cache reuse, and states the reproducibility implications. A
  wall-clock date never silently masquerades as evidence of a new revision.
- REQ-241-02: The policy is applied to every maintained image-building path
  (adversary, asteria-game, asteria-stub, sidecar, tracer-sidecar,
  tx-generator). Newly produced artifacts carry source revision provenance
  and no longer silently default to the 1970 epoch.
- REQ-241-03: Permanent verification inspects the emitted OCI configuration,
  not only Nix source. RED on the current harness and GREEN after the change
  are demonstrated; controls reject epoch/default metadata and missing or
  mismatched source provenance.
- REQ-241-04: Warm-cache/repeated builds and publication-skip behavior are
  exercised. Reused artifacts are reported honestly, existing immutable
  identities are preserved, and corrected artifacts receive new identities
  selected by the intended consumer configuration.
- REQ-241-05: The actual component packaging path (`nix build .#docker-image`
  from a component directory) runs for representative affected images with
  retained source SHA, timestamp inputs, OCI creation dates, digests, and
  verification output. The check is wired into local and hosted CI.
- REQ-241-06: The operator build/publish procedure and the remaining
  Antithesis boundary are documented. The live age warning is not claimed
  resolved without a subsequent run consuming the corrected artifacts.

## Success criteria

- Building any maintained component image yields OCI `created` equal to the
  documented source time, never the epoch default.
- OCI labels carry the source revision, source URL, version, and creation
  time, matching the inputs that produced the artifact.
- The shipped verifier exits non-zero on an epoch-dated image, on missing
  provenance labels, and on revision mismatch; it exits zero on a corrected
  artifact and prints the observed identity.
- A second consecutive build of identical sources reuses the identical store
  path and reports reuse rather than minting a new identity.
- The publisher still skips already-published immutable tags and
  digest-only pins; corrected sources produce new tags the consumer
  configuration selects.
- `nix develop --quiet -c just ci` passes, including the new verification.

## Scope boundaries

- Do not rotate Cardano versions, change the mixed-version topology, or
  complete the daily upstream-HEAD epic #214–216.
- Do not change MOOG, rebuild only its config image, disable the Antithesis
  property, or add a dummy fresh image to make the aggregate check green.
- Do not retag old artifacts as proof that new software was tested.
- Do not timestamp from wall-clock build time as if it were source freshness.
- No unrelated component behavior, deployment, or dependency changes.
- The MOOG-built config image stays outside this harness; its freshness is a
  documented residual, not a claim of this ticket.
