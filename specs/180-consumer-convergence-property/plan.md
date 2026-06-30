# Implementation Plan: Score the amaru-consumer's convergence

**Branch**: `feat/consumer-convergence-property` | **Date**: 2026-06-30 | **Spec**: `spec.md`
**Input**: `specs/180-consumer-convergence-property/spec.md` (issue #180, epic #182)

## Status

**Completed**: #179 merged to main. Worktree + spec. tracer-sidecar assertion
framework + test harness understood.
**Current**: Plan — TDD red-first via the tracer-sidecar test suite.
**Blockers**: None.

## Summary

Add a scored Antithesis property to `tracer-sidecar` that fires once the
`amaru-consumer` has advanced past its first observed tip **and** reached a
tip also seen on a producer — i.e. it converged onto the producers' chain
through amaru. Because the consumer is network-isolated to amaru-only (#179),
any advance is necessarily *via amaru*, so this property scores the exact
liveness gap the cluster-wide fork-tree/no-critical probes miss (a stuck
consumer goes stale, not forked).

## The framework (verified)

- `mkSpec :: Int -> Spec` builds `[Rule]` via `sometimes` / `alwaysOrUnreachable`
  / `observe`. Each `sometimes name f` **declares** an Antithesis `must_hit`
  assertion and emits *hit* the first time `f` returns True; if never hit, the
  Antithesis report scores it as a failure.
- `State { unreachedAssertions, forkTree, … }`; `LogMessage { host, kind,
  details = AddedToCurrentChain{newtip,…}, sev, json }`. Producer detection
  already uses `stripSuffix ".example" host == "pN"`.
- Test harness: `test/Spec.hs` golden (`input.jsonl` → `output.jsonl`,
  comparing emitted `antithesis_assert` JSON) + `ForkTreeSpec.hs` hspec. This
  is the RED→GREEN driver — **no cluster, no Antithesis spend** for the logic.

## CRITICAL design point — the property must be conditional

`sometimes` declares a `must_hit` assertion **unconditionally**. If we add the
convergence property to `mkSpec` flat, it would also be declared for
`cardano_node_master` (which has **no** `amaru-consumer`) → never hit → a
**spurious scored failure** on every master run. So:

- `mkSpec` takes the consumer identity as a parameter (e.g.
  `mkSpec :: Int -> Maybe Text -> Spec`, the `Maybe Text` = consumer host or
  Nothing). The convergence property is added **only when a consumer host is
  configured**.
- `App.hs` reads it from a new env var (e.g. `AMARU_CONSUMER_HOST`); absent →
  `Nothing` → property not declared (master runs unaffected).
- Only the `cardano_amaru` compose sets `AMARU_CONSUMER_HOST=amaru-consumer.example`.

This is the difference between "a new scored property" and "a regression that
reddens an unrelated testnet". It is non-negotiable.

## The property

State gains:
- `producerTipHashes :: Set BlockHash` — every `newtip` hash seen on a producer.
- `consumerFirstLen :: Maybe Int` — consumer's first observed chain length.

State updater rule (when consumer host is configured): on each
`AddedToCurrentChain{newtip}`:
- producer host → insert `newtip.hash` into `producerTipHashes`.
- consumer host → set `consumerFirstLen` if unset.

Property: `sometimes "amaru-served consumer reached producer tip"` fires when a
consumer `AddedToCurrentChain` has `newtip.chainLength > consumerFirstLen`
**and** `newtip.hash ∈ producerTipHashes` (accumulated across the run, so a
producer "moving past" the point doesn't lose it).

Why both clauses: length-advance proves it pulled blocks (only possible via
amaru, since isolated); hash-on-a-producer proves it converged onto the
producers' chain (not a private fork). A stuck consumer never advances → never
hit → scored failure. (Optionally also an `alwaysOrUnreachable "amaru-consumer
not on a divergent fork"` — decide during S1 if it adds signal over the
existing `forkTreeProbe`; default to just the `sometimes` to keep it minimal.)

## Slices (bisect-safe, one commit each)

### Slice 1 — the conditional convergence property + RED→GREEN tests

- `mkSpec` takes the consumer host; `App.hs` reads `AMARU_CONSUMER_HOST`.
- Add the state fields + updater + `sometimes` property (only when configured).
- **TDD (RED first, mandatory)**: add hspec cases (and/or extend the golden)
  feeding synthetic `AddedToCurrentChain` streams:
  - **hit case**: consumer advances and a tip hash matches a producer tip →
    property emitted hit. Write this test FIRST, watch it FAIL (property
    absent), then implement.
  - **not-hit case**: consumer stuck at seed → property declared, not hit.
  - **no-consumer case**: `Nothing` → property not declared at all (guards the
    master-regression).
- **Gate**: `cd components/tracer-sidecar && cabal test` (or `nix build
  .#tracer-sidecar-tests`) green; `fourmolu`/`hlint`/`cabal-fmt` clean; the
  existing golden output stays otherwise stable.
- One commit: property + tests together.

### Slice 2 — wire it into the cardano_amaru cluster + image bump

- `testnets/cardano_amaru/docker-compose.yaml`: set
  `AMARU_CONSUMER_HOST=amaru-consumer.example` on the `tracer-sidecar` service,
  and **bump the `tracer-sidecar` image tag** to a `feat/consumer-convergence-
  property` commit SHA (constitution: image-tag hygiene → `publish-images`
  rebuilds with the property). Leave `cardano_node_master` unset (no consumer).
- **Gate**: `INTERNAL_NETWORK=false docker compose -f
  testnets/cardano_amaru/docker-compose.yaml config` validates; image spelling
  constraint preserved (digest/tag rules).

### Finalization — AC3, the one launched run

After S1+S2 are green and merged-ready and `publish-images` has built the new
`tracer-sidecar` image: launch **one** `cardano_amaru` run and confirm the
`amaru-served consumer reached producer tip` property appears in the report
(`anti properties <run_id>`). This is the only Antithesis spend for #180, and
it happens **after** the logic is green locally and the image is published —
never before. (The fault-scoped green run is #181.)

## Risks

- **R1 (master regression)**: forgetting the conditional → spurious failure on
  `cardano_node_master`. Mitigated by the `no-consumer` test case (S1) + only
  `cardano_amaru` setting the env (S2).
- **R2 (chain-length field)**: confirm `AddedToCurrentChain` carries a usable
  chain length / the `Tip` the ForkTree already parses (`newTipSelectView`);
  reuse the ForkTree's existing tip parsing rather than re-deriving.
- **R3 (golden churn)**: the new property changes emitted outputs only when a
  consumer host is configured; the existing `input.jsonl` (master-shaped, no
  consumer) golden should be unchanged. Add consumer fixtures in new test cases,
  don't perturb the existing golden.

## Test strategy

The tracer-sidecar hspec/golden suite is the RED→GREEN harness — proven locally
and deterministically. AC3 (surfaces in a report) is the only step needing a
real run, performed last.
