---

description: "Task list for 004-yaci-store"
---

# Tasks: yaci-store as Antithesis N2N consumer

**Input**: Design documents from `/specs/004-yaci-store/`
**Prerequisites**: [plan.md](./plan.md) (required), [spec.md](./spec.md) (required for user stories), [research.md](./research.md), [data-model.md](./data-model.md), [contracts/compose-topology.md](./contracts/compose-topology.md), [quickstart.md](./quickstart.md)

**Tests**: This feature ships no Haskell/Java unit tests — its tests are operational (`just smoke-test`, the SC-001..SC-005 commands in `quickstart.md`, and the constitution's 1h Antithesis hard gate). Tasks below schedule those operational checks at the points where they are meaningful.

**Organization**: Tasks are grouped by user story to enable independent verification. The MVP is User Story 1 (yaci-store joins the testnet and tracks the chain).

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3)
- All file paths are absolute under `/code/cardano-node-antithesis-issue-75/`.

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Pre-implementation discovery — confirm the assumptions the contract builds on.

- [ ] T001 Pull the upstream image and capture its `sha256` digest, recording it in a scratch note for use in T010: `docker pull docker.io/bloxbean/yaci-store:2.0.0 && docker inspect --format='{{index .RepoDigests 0}}' docker.io/bloxbean/yaci-store:2.0.0`. Reference: [research.md R-001](./research.md), [contracts/compose-topology.md "Image digest pin"](./contracts/compose-topology.md).
- [ ] T002 Confirm the genesis filenames the configurator emits into `p1-configs:/configs/configs/`: bring the configurator service up alone via `docker compose -f testnets/cardano_node_master/docker-compose.yaml up -d configurator`, then `docker run --rm -v cardano-node-master_p1-configs:/configs:ro alpine ls /configs/configs/`. If filenames differ from the contract's placeholders (`byron-genesis.json` etc.), record the real names. Reference: [contracts/compose-topology.md "Genesis filename confirmation"](./contracts/compose-topology.md).
- [ ] T003 [P] Confirm whether the upstream image ships `curl` or `wget` for the healthcheck: `docker run --rm --entrypoint=sh docker.io/bloxbean/yaci-store:2.0.0 -c 'which curl wget'`. Pick whichever is present for T011. Reference: [data-model.md "Healthcheck entity"](./data-model.md).

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: None for this feature. There are no shared prerequisites between user stories — every story is verified against the same single compose stanza, and the stanza itself is implemented once in Phase 3.

**Checkpoint**: Setup tasks T001..T003 complete; assumptions verified. Implementation can begin.

---

## Phase 3: User Story 1 — yaci-store joins the testnet and tracks the chain (Priority: P1) 🎯 MVP

**Goal**: Add the yaci-store service to the testnet so it comes up cold, follows the chain over N2N from a relay, and survives a relay restart.

**Independent Test**: After `just up cardano_node_master`, yaci-store reaches a tip slot equal to or greater than the relay's tip slot within 5 minutes of the network producing its first block, and continues to follow after a `docker compose restart relay1` (per `quickstart.md` step 2 + step 4).

### Implementation for User Story 1

- [ ] T010 [US1] Append the `yaci-store` service stanza to `/code/cardano-node-antithesis-issue-75/testnets/cardano_node_master/docker-compose.yaml`, immediately after the `oura` service. Use the exact YAML in [contracts/compose-topology.md](./contracts/compose-topology.md), substituting the `sha256` digest from T001 and the genesis filenames from T002. Do **not** add any new named volume — reuse `p1-configs`.
- [ ] T011 [US1] In the same stanza, set the healthcheck command to whichever of `curl` / `wget` was confirmed by T003. Default in the contract is `wget`; switch to `curl` only if T003 found `curl` and not `wget`.
- [ ] T012 [US1] Run the local smoke recipe: `just down cardano_node_master && just up cardano_node_master`. Observe that all existing services still start (no regression), then check that yaci-store reaches `healthy` within 1 minute (`docker compose -f testnets/cardano_node_master/docker-compose.yaml ps yaci-store`). Reference: [quickstart.md step 1](./quickstart.md), SC-003.
- [ ] T013 [US1] Verify SC-001 (yaci-store reaches relay tip within 5 minutes) per [quickstart.md step 2](./quickstart.md). Capture the relay tip slot, then capture yaci-store's reported tip from `just logs yaci-store`. Record the actual time-to-tip in the PR description.
- [ ] T014 [US1] Verify the relay-restart acceptance scenario (User Story 1 #2) per [quickstart.md step 4](./quickstart.md): `docker compose ... restart relay1`, then confirm yaci-store reconnects without exiting.
- [ ] T015 [US1] Verify SC-004 (clean teardown): `just down cardano_node_master`; confirm no yaci-store volumes or containers remain. Reference: [quickstart.md step 5](./quickstart.md).

**Checkpoint**: User Story 1 is functional and testable independently. The compose stanza is in place, the testnet still comes up, and yaci-store is following the chain. From this checkpoint the feature already delivers value (an additional N2N consumer is exercised by every local `just up`); the remaining stories raise it to merge-ready.

---

## Phase 4: User Story 2 — yaci-store survives Antithesis fault injection without corrupting the verdict (Priority: P1)

**Goal**: Confirm that yaci-store under fault injection produces no new findings versus the prior `main` baseline, and document its SIGTERM behaviour.

**Independent Test**: A 1-hour Antithesis duration on `004-yaci-store` produces `findings_new ≤ baseline` (constitution hard gate). yaci-store's exit code on SIGTERM is recorded.

### Implementation for User Story 2

- [ ] T020 [US2] Capture the current `main` 1h Antithesis baseline finding count: locate the most recent green `cardano-node.yaml` 1h run on `main` via `gh run list -R cardano-foundation/cardano-node-antithesis -w cardano-node.yaml --branch main --limit 5`, open the report, record the finding count (and any stable finding IDs we already accept). Note: this is read-only against `main`.
- [ ] T021 [US2] Verify SIGTERM behaviour locally per [quickstart.md step 6](./quickstart.md): `docker stop yaci-store` and capture the exit code. Expected `0`. If non-zero, do **not** proceed to T022 — instead, file a follow-up issue and either (a) configure `stop_signal: SIGINT` / `stop_grace_period: 30s` in the compose stanza or (b) coordinate a harness whitelist. Reference: FR-009, [research.md R-006](./research.md).
- [ ] T022 [US2] After T010..T015 land on the branch and `publish-images` is green, dispatch a 1h Antithesis run: `gh workflow run cardano-node.yaml --ref 004-yaci-store --field duration=1h`. Record the run URL.
- [ ] T023 [US2] When the 1h run completes, compare its findings against the baseline from T020 using the `antithesis-triage` skill. For every new finding: (a) attribute it to a container by name; (b) if attributable to yaci-store, file a follow-up issue and either fix or whitelist with a documented reason. PR cannot merge until `findings_new ≤ baseline`.
- [ ] T024 [US2] Update the PR description with: the baseline finding count, the run URL from T022, the actual finding count, the diff, and any follow-up issue numbers.

**Checkpoint**: User Story 2 satisfied. The constitution's hard gate is met or the follow-ups are filed. Merge is unblocked from the testing axis.

---

## Phase 5: User Story 3 — yaci-store's image is reproducible and reachable from Antithesis (Priority: P2)

**Goal**: Confirm that the digest pinned in compose is mechanically resolvable from a fresh machine, so scheduled Antithesis runs do not break with `manifest unknown`.

**Independent Test**: A `docker pull` of the digest from a machine that has never seen the upstream image succeeds.

### Implementation for User Story 3

- [ ] T030 [US3] On a host that has not yet pulled `docker.io/bloxbean/yaci-store`, run `docker pull docker.io/bloxbean/yaci-store@sha256:<DIGEST>` where `<DIGEST>` is the value committed in T010. Confirm success. (Locally: `docker rmi docker.io/bloxbean/yaci-store:2.0.0` to force a re-pull from a fresh state.)
- [ ] T031 [US3] After pushing the branch (Phase 6), verify the `publish-images` workflow run on the PR is green. The workflow does **not** rebuild yaci-store (it is upstream); confirm it does not error on the new compose entry. Reference: [research.md R-002](./research.md).

**Checkpoint**: User Story 3 satisfied. The image reference is durable.

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Things that affect the PR as a whole.

- [ ] T040 [P] Update `/code/cardano-node-antithesis-issue-75/testnets/cardano_node_master/README.md` (if present) to mention yaci-store as the second N2N consumer. If the README does not exist, skip this task.
- [ ] T041 [P] Run `just smoke-test cardano_node_master` end-to-end on the branch one final time before pushing. This is the same smoke test CI runs.
- [ ] T042 Push the branch: `git push -u origin 004-yaci-store`. Reference: workflow rule "Always push for review".
- [ ] T043 Open the PR with `gh pr create`, body sourced from this directory's specs (link to spec.md, plan.md, contracts, quickstart). Title: `feat(testnet): add yaci-store N2N consumer`. Labels: `enhancement`. Assignee: `paolino`. Refs #75.
- [ ] T044 Add the PR to the planner board (Antithesis category, Work ownership) per the workflow rule. Use `gh api graphql` per the workflow skill's instructions.

---

## Dependencies & Execution Order

### Phase dependencies

- Phase 1 (Setup) → Phase 3 (US1).
- Phase 3 (US1) → Phase 4 (US2). Phase 4 needs the compose change to be on the branch and `publish-images` green before dispatching the 1h Antithesis run.
- Phase 3 (US1) → Phase 5 (US3). Phase 5 needs the digest committed.
- Phase 4 + Phase 5 → Phase 6 polish. Specifically, T043 (open PR) lands after the 1h run is at least dispatched (the PR description references it); T024 updates the PR after the run completes.

### User story dependencies

- US1 has no dependency on other stories.
- US2 depends on US1's compose change being on the branch.
- US3 depends on US1's digest being committed.
- US2 and US3 can run in parallel after US1 lands.

### Within each user story

- Implementation tasks are sequential within US1 because they modify the same file (`docker-compose.yaml`).
- US2 tasks T020 (baseline read) and T021 (local SIGTERM check) can run in parallel; T022/T023/T024 are sequential.
- US3 tasks T030 and T031 can run in parallel.

### Parallel opportunities

- T003 runs in parallel with T001 + T002 (different containers, different commands).
- T020 ‖ T021 (US2).
- T030 ‖ T031 (US3).
- T040 ‖ T041 (Phase 6 polish).

---

## Parallel example: User Story 2

```bash
# T020 (baseline) and T021 (SIGTERM check) are independent:
gh run list -R cardano-foundation/cardano-node-antithesis -w cardano-node.yaml --branch main --limit 5 &
( just up cardano_node_master && sleep 60 && docker stop yaci-store && \
  docker inspect yaci-store --format '{{.State.ExitCode}}' )
wait
```

---

## Implementation strategy

### MVP first (User Story 1 only)

1. Phase 1: Setup (T001..T003) — verify assumptions.
2. Phase 3: US1 (T010..T015) — land the compose change and verify it works locally.
3. STOP and VALIDATE: at this point the feature is merge-ready *operationally* but not yet *Antithesis-verified*. Do **not** push for merge yet.

### Verification (US2 + US3 in parallel)

4. Phase 4 (US2) and Phase 5 (US3) can run in parallel by different operators or sequentially.
5. T020 reads baseline, T022 dispatches the 1h run, T023 + T024 close the loop.
6. T030/T031 confirm the image pin is durable.

### Polish and PR

7. Phase 6 (T040..T044) — final smoke test, push, PR, planner.
8. Merge gate: constitution's verify-before-merge sequence (publish-images green → 1h Antithesis findings ≤ baseline → `mcp__merge-guard__guard-merge` with `mergeMethod: rebase`).

---

## Notes

- [P] = different files / different commands, no dependencies.
- The single source code touch in this entire feature is `testnets/cardano_node_master/docker-compose.yaml`. Everything else is documentation, verification, and process.
- Every task references the document or section that owns its details, per the [Tasks reference contracts](memory) workflow rule.
- Do **not** modify `tx-generator`, `asteria-player`, or any composer command — yaci-store is additive coverage (FR-010).
- Do **not** add a new component under `components/` — yaci-store is an upstream image (FR-005, R-001).
