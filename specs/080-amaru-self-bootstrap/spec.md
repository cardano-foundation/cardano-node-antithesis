# Feature Specification: Amaru relays self-bootstrap

**Feature Branch**: `080-amaru-self-bootstrap`
**Created**: 2026-05-02
**Status**: Draft
**Input**: User description: "collapse the standalone bootstrap-producer service into the amaru-relay containers so Antithesis setup completes immediately and the bootstrap work happens during the test phase"

## Context

Antithesis enforces a 6-minute deadline for "setup complete". The current
`testnets/cardano_amaru_epoch3600/` topology gates `amaru-relay-1` and
`amaru-relay-2` on a one-shot `bootstrap-producer` service via
`depends_on: bootstrap-producer:service_completed_successfully`. The bootstrap
step is unbounded — it depends on a randomized initial state distribution and
must catch the producer chain through to a recent epoch (potentially 2–3
epochs of header/snapshot work). When that work cannot finish within 6
minutes, Antithesis reports `No 'setup complete' event received` and the
campaign is aborted before any test phase executes.

Reference incident: testRunId
`667f319955a2956182d9bcbcc4a1b28b87def6589c1c9786ae071b4992eb22fa`,
[GH run 25237677229][run], [Antithesis report][report].

[run]: https://github.com/cardano-foundation/cardano-node-antithesis/actions/runs/25237677229
[report]: https://cardano.antithesis.com/report/eWsMhCd8q5PIAI-xv0CobduX/YFRkIxgk2WFp-wC2fB-FIV2zNzoYbkrNHPDYjs7YrIs.html

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Antithesis setup completes regardless of initial state distribution (Priority: P1)

As an operator submitting an Antithesis test against the Amaru testnet, I
want compose `up` to return within Antithesis's setup deadline regardless of
how far behind the producer cluster's initial state happens to be, so that
the test phase always runs and I get either a clean campaign or a meaningful
property report — never a `setup_error`.

**Why this priority**: Without this, every run whose initial state requires
more than ~5 minutes of Amaru bootstrap is wasted: no findings, no logs
worth triaging, no signal about Amaru itself. This is the precondition for
*any* Amaru observation in Antithesis.

**Independent Test**: Submit a test run via the existing GitHub Actions
workflow against the modified testnet. The Antithesis report MUST show
either `Completed` or a normal property report (passed/failed mix) — it MUST
NOT show `setup_error: No 'setup complete' event received`. Repeat across at
least three submissions with the default randomized fault settings.

**Acceptance Scenarios**:

1. **Given** a fresh Antithesis test submission against the Amaru testnet,
   **When** Antithesis runs `compose up`, **Then** all containers reach
   their `service_started` state within 6 minutes and the `setup complete`
   event is emitted.
2. **Given** the slowest plausible initial state (producer cluster needs
   2–3 epochs of catch-up), **When** the test campaign starts, **Then**
   setup is already complete and the campaign begins exercising properties
   while the Amaru relays continue bootstrapping inside the test phase.

---

### User Story 2 - Sidecar still gates chainpoint workflow on Amaru readiness (Priority: P1)

As the existing sidecar that records chainpoints, I must continue to wait
for both Amaru relays to be fully bootstrapped (ledger + chain DBs present
and `amaru run` started) before I begin my chainpoint workflow, so that the
properties downstream of the chainpoint workflow remain meaningful.

**Why this priority**: The sidecar's `AMARU_STARTUP_REQUIRED=true` contract
predates this change and is the only readiness signal the rest of the
testnet relies on. Breaking it would silently invalidate every property
that compares cardano-node and Amaru chain views.

**Independent Test**: After setup completes, the sidecar's chainpoint
workflow MUST NOT progress past its initial wait until BOTH
`/amaru-startup/amaru-relay-1.started` and
`/amaru-startup/amaru-relay-2.started` markers exist. Verify by inspecting
sidecar logs in a successful campaign — chainpoint operations are
timestamped strictly after both startup markers.

**Acceptance Scenarios**:

1. **Given** setup has completed and the test phase is running, **When**
   only one Amaru relay has finished its in-container bootstrap, **Then**
   the sidecar's chainpoint workflow is still waiting.
2. **Given** both Amaru relays have written their startup markers,
   **When** the sidecar polls, **Then** the chainpoint workflow proceeds
   normally as it does today.

---

### User Story 3 - Bootstrap continues to tolerate Antithesis fault injection (Priority: P2)

The current `bootstrap-producer` retries internally when it observes the
transient failures Antithesis produces (snapshot copy errors under
filesystem faults, partial immutable history under network faults — exit
codes 1/2/5/7 from `/bin/bootstrap-producer`). The merged in-relay
bootstrap MUST preserve this retry semantics so that fault injection during
bootstrap does not cause the relay container to give up and the testnet to
silently degrade to "Amaru not running".

**Why this priority**: Without retry, the change trades a `setup_error`
class of failure for a "container exited" class of failure. The campaign
would still run but Amaru properties would not exercise — a subtler and
worse signal.

**Independent Test**: Run the testnet locally with podman-compose and
inject a `chmod 000` on the producer state mid-bootstrap. The amaru-relay
container MUST log a transient failure and retry, eventually completing
once the fault is removed. The container MUST NOT exit non-zero on
transient codes.

**Acceptance Scenarios**:

1. **Given** the relay is mid-bootstrap and `/bin/bootstrap-producer`
   exits 1, 2, 5, or 7, **When** the entrypoint observes the exit, **Then**
   it sleeps `AMARU_BOOTSTRAP_RETRY_SECONDS` and tries again from a fresh
   snapshot copy.
2. **Given** the relay is mid-bootstrap and `/bin/bootstrap-producer`
   exits with any other non-zero code, **When** the entrypoint observes
   the exit, **Then** the last attempt log is surfaced and the relay
   container exits with that code.

---

### Edge Cases

- **Both relays bootstrap concurrently against different producers**: each
  relay reads producer state from its paired cardano-node (relay-1 ↔ p1,
  relay-2 ↔ p2), mounted read-only. Concurrent reads against distinct
  producer state volumes are independent and MUST be safe.
- **Antithesis kills a relay mid-bootstrap and restarts it**: the
  entrypoint MUST be idempotent — on restart, the working snapshot copy
  is re-taken from a fresh producer snapshot; the final ledger/chain DBs
  MUST not be left half-written such that `amaru run` starts against
  corrupt state.
- **Producer is itself behind**: the relay's bootstrap retry loop MUST
  keep trying; it MUST NOT escalate "snapshot incomplete" to a hard
  failure. Today's retry codes (1/2/5/7) cover this.
- **Disk pressure from per-relay bootstrap working copies**: each relay
  now keeps its own scratch copy of producer immutable/ledger/volatile
  dirs (previously a single shared copy in `bootstrap-state`). Storage
  budget MUST account for 2× the per-relay working set.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The `cardano_amaru_epoch3600` testnet MUST NOT include a
  standalone `bootstrap-producer` service.
- **FR-002**: Each `amaru-relay-N` container MUST perform its own
  bootstrap from the producer-cluster state it is paired with (relay-1 ↔
  p1, relay-2 ↔ p2), and then `exec amaru run …` in the same container.
- **FR-003**: The `amaru-relay-N` containers MUST start as soon as their
  upstream cardano-node producer is `service_started`. They MUST NOT
  block on `service_completed_successfully` from any other service.
- **FR-004**: The `amaru-relay-N` entrypoint MUST retry transient
  bootstrap failures (`/bin/bootstrap-producer` exit codes 1, 2, 5, 7)
  with the same `AMARU_BOOTSTRAP_RETRY_SECONDS` semantics as the current
  `bootstrap-producer` service.
- **FR-005**: The `amaru-relay-N` entrypoint MUST write the existing
  `/amaru-startup/amaru-relay-N.started` marker only AFTER the bootstrap
  step has completed and immediately before `exec amaru run`.
- **FR-006**: The shared `amaru-bundle` and `bootstrap-state` volumes
  MUST be removed; per-relay scratch state lives on the existing per-relay
  state volumes (`a1-state`, `a2-state`).
- **FR-007**: The sidecar's `AMARU_STARTUP_REQUIRED=true` /
  `AMARU_STARTUP_DIR` / `AMARU_RELAYS` configuration MUST remain
  unchanged in shape and semantics.
- **FR-008**: Antithesis "setup complete" MUST be emitted as soon as
  every container is `service_started`. Bootstrap progress inside the
  relay containers MUST happen in the test phase, not the setup phase.

### Key Entities *(include if feature involves data)*

- **Producer state volume (`pN-state`)**: read-only input to the relay
  bootstrap; contains `immutable/`, `ledger/`, `volatile/`,
  `protocolMagicId`, `lock`. Identical to today's `bootstrap-producer`'s
  `/live` mount, but mounted into the relay instead of into a separate
  service.
- **Per-relay state volume (`aN-state`)**: writable working area mounted
  at `/srv/amaru` in the relay; holds both the bootstrap scratch copy and
  the final `ledger.testnet_42.db` / `chain.testnet_42.db` /
  `nonces.json` consumed by `amaru run`.
- **Startup marker (`/amaru-startup/amaru-relay-N.started`)**: file
  written by the relay entrypoint after bootstrap, before `amaru run`.
  Read by the sidecar's chainpoint workflow.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Across 5 consecutive Antithesis test submissions against
  the Amaru testnet with default fault settings, ZERO submissions return
  `setup_error: No 'setup complete' event received`.
- **SC-002**: Antithesis "setup complete" is emitted within 90 seconds
  of `compose up` start (well below the 6-minute deadline) regardless of
  initial state distribution.
- **SC-003**: For Antithesis runs of 1-hour duration, the sidecar's
  chainpoint workflow proceeds in 100% of runs (i.e. both Amaru relays
  finish their in-test-phase bootstrap within the first hour for any
  initial state we have observed).
- **SC-004**: When `/bin/bootstrap-producer` exits 1/2/5/7 during a
  relay's bootstrap, the relay container is observed to retry at least
  once (i.e. transient failures do not propagate to "container exited
  with non-zero" findings in Antithesis).

## Assumptions

- Each Amaru relay reads producer state from exactly one producer
  cardano-node, mirroring the existing `--peer-address` topology
  (relay-1 ↔ `p1`, relay-2 ↔ `p2`). Cross-pairing or shared sources are
  out of scope.
- Per-relay disk budget under Antithesis can absorb a second working
  copy of the producer immutable+ledger+volatile state. The reference
  6-min snapshot showed bootstrap-producer's disk read footprint at
  ~70 MB block_read; per-relay scratch copies at this scale are
  acceptable. If the actual on-disk working set is materially larger,
  this assumption needs revisiting.
- The `service_completed_successfully` semantics for amaru-relay's
  `depends_on: bootstrap-producer` is the only place that gate exists in
  the testnet — no other service references `bootstrap-producer`.
- The Antithesis 6-minute setup deadline is not negotiable and the
  bootstrap workload is genuinely epoch-bounded (cannot be made fast
  enough to fit), so the architectural fix (move bootstrap out of setup)
  is the only viable path.
- The change is scoped to the `cardano_amaru_epoch3600` testnet; other
  testnets (`cardano_node_master`, `cardano_node_adversary`, etc.) are
  unaffected.
