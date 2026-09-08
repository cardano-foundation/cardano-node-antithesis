# Tasks — Issue 241

## Slice S1 — Source-time provenance for all maintained images

- [X] T241-1 Apply the source-time policy: new `components/image-meta`
  helper; `created` + OCI labels in all six `nix/docker-image.nix`;
  `path:../image-meta` wiring in all six `flake.nix` (lock the new inputs).
- [X] T241-2 Ship `scripts/check-image-provenance.sh` per the CLI contract;
  prove it rejects epoch metadata, missing labels, and revision mismatch,
  and accepts a corrected artifact with a printed identity.
- [X] T241-3 Ship `tests/test-image-provenance.sh` per the proof contract,
  covering verifier classes plus publisher sanitize/skip controls with a
  fake-`docker` shim; wire `just check-image-provenance` into `just ci`
  and the hosted workflow.
- [X] T241-4 Run the real packaging path for representative images
  (asteria-stub first, then tx-generator; a Haskell-built image only
  within the remaining execution budget): retain source SHA, timestamp
  inputs, OCI dates, digests, and verifier output; exercise a warm-cache
  rebuild and report reuse honestly.
- [X] T241-5 Document the timestamp policy, the operator build/publish
  procedure, and the residual Antithesis boundary (MOOG config image,
  old pins, required consuming run); connect corrected tags to the
  consumer Compose selection without retagging old bytes.

## Orchestrator finalization (ticket owner)

- [ ] Independently replay the RED controls and retain raw logs.
- [ ] Independently run the clean verifier and the full ticket gate.
- [ ] Verify the hosted workflow reaches the provenance check.
- [ ] Record complete evidence in the PR body.
- [ ] Obtain publication/merge authorization through the Q/A protocol
      where the workflow requires it; never merge or publish images
      without it.
- [ ] Hand back result, commit/tree identity, policy decision, artifact
      receipts, RED/GREEN/cache controls, gate exit, audit verdict, PR
      text/URL, and residual live validation.
