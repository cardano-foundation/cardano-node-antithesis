# Feature Specification: Score the amaru-consumer's convergence as an Antithesis property

**Feature Branch**: `feat/consumer-convergence-property`
**Created**: 2026-06-30
**Status**: Draft
**Input**: GitHub issue #180 (parent epic #182, depends on merged #179)

## Context

#179 added an amaru-fed `amaru-consumer` cardano-node and a **local** smoke
that fails closed unless the consumer advances past its seed and matches a
producer tip. But that proof lives only in `scripts/smoke-test.sh` — it is
**not** a scored Antithesis property. As established while triaging the
(aborted) shake-out: the consumer's logs are collected (it forwards to
cardano-tracer; `tracer-sidecar` tails its dir), and it's folded into the
cluster-wide `forkTreeProbe` and `"no critical logs"` — but those only catch
*divergence* and *crashes*. A consumer that silently froze because amaru
stopped serving would go **stale on the same chain**: no fork, no critical →
the run passes green while the boundary actually failed. #180 closes that gap.

## User Scenarios & Testing

### User Story 1 — A scored property proves the consumer converged via amaru (P1)

As a tester, I read the Antithesis report and see a property that proves the
amaru-fed consumer advanced past its seed and converged with the producers —
so a green run is real evidence amaru served it.

**Why P1**: Without it, a stuck consumer (amaru not serving) is invisible to
Antithesis. This property is the whole point of the epic's Antithesis arc.

**Independent Test**: Feed `tracer-sidecar` a synthetic log stream (golden /
hspec) where the consumer advances and reaches a producer tip → the
`sometimes "amaru-served consumer reached producer tip"` assertion is **hit**.
Feed a stream where the consumer never reaches a producer tip → the assertion
is **declared but not hit** (which Antithesis scores as a failure).

**Acceptance Scenarios**:
1. **Given** the consumer's `AddedToCurrentChain` tips advance past its first
   observed tip **and** a tip hash also seen on a producer, **When**
   `tracer-sidecar` processes the stream, **Then** it emits the
   `amaru-served consumer reached producer tip` SDK assertion as hit.
2. **Given** the consumer's tip never matches a producer tip (stuck at seed),
   **When** the stream ends, **Then** the assertion is declared (`must_hit`)
   and **not** hit — the Antithesis-scored failure signal.

### User Story 2 — The property is live in the deployed image (P1)

As a maintainer, I need the new property compiled into the `tracer-sidecar`
image the cluster actually runs, so it surfaces in real runs.

**Why P1**: A property only in source, with a stale image tag, never runs.

**Independent Test**: The `tracer-sidecar` image tag in the `cardano_amaru`
compose is bumped to a branch commit SHA (so `publish-images` rebuilds it),
and `docker compose config` validates.

### User Story 3 — The property surfaces in a real report (P1)

As a tester, I confirm the property actually appears in a launched run's
report (not absent/removed).

**Why P1**: AC3 is explicit — the property must be *verified against a
launched run*. This is the one unavoidable Antithesis spend for #180, done
**only after** US1 is green locally and US2's image is published.

**Independent Test**: Launch one `cardano_amaru` run and confirm the
`amaru-served consumer reached producer tip` property appears in the report
(`anti properties <run_id>` / the triage report).

## Requirements

### Functional Requirements

- **FR-001**: `amaru-consumer` MUST be in the tracer-sidecar's observed set —
  i.e. its `AddedToCurrentChain` events feed the same machinery as producers.
  (Already true via #179's tracer wiring; verify, don't re-plumb.)
- **FR-002**: A scored property MUST reflect convergence-through-amaru: a
  `sometimes "amaru-served consumer reached producer tip"` that fires once the
  consumer's tip has advanced past its first observed tip **and** equals a tip
  also observed on a producer (p1/p2/p3). Optionally an `alwaysOrUnreachable`
  guarding a regression.
- **FR-003**: The property MUST be exercised by the tracer-sidecar test
  harness (hspec/golden) with BOTH a hit case and a not-hit case — proven RED
  before GREEN.
- **FR-004**: The `tracer-sidecar` image tag in `testnets/cardano_amaru/
  docker-compose.yaml` MUST be bumped to a branch commit SHA so
  `publish-images` rebuilds the image with the property (constitution: image
  tag hygiene).
- **FR-005**: The property MUST be verified to SURFACE in a launched
  `cardano_amaru` run's report (AC3) — after FR-002/003 are green and FR-004's
  image is built.

### Constraints

- **C-001**: No fault scoping of the assertion — that's #181.
- **C-002**: Don't touch amaru's own-tip observability (cna#168).
- **C-003**: The property must not regress the existing fork-tree /
  no-critical / per-producer assertions (golden output stays otherwise stable).

## Success Criteria

- The tracer-sidecar test suite has a hit case and a not-hit case for the new
  property, both proven (RED seen before GREEN).
- A launched `cardano_amaru` run's report shows the property.
- A stuck consumer (amaru not serving) would make the property **not hit** —
  the gap #179's local smoke caught, now scored by Antithesis.

## Out of Scope

- Fault scoping / which faults may break it (#181).
- The fault-scoped green run (#181).
