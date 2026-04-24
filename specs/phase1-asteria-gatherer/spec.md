# Feature Specification: Asteria Gatherer — Phase 1 Eager-Agent Workload

**Feature Branch**: `feat/asteria-phase1-gatherer`
**Created**: 2026-04-24
**Status**: Draft
**Input**: "Now that the composer infrastructure is clean, bring up a real-application workload driven by eager agents. Start with txpipe/asteria (the bot-versus-bot Cardano game): admin bootstraps the game state, N gatherer bots pursue fuel pellets in parallel under Antithesis fault injection, and both continuous and post-workload invariants are asserted via the Antithesis SDK."
**Parent issue**: #56 (sub of umbrella #55)

## User Scenarios & Testing *(mandatory)*

### User Story 1 — A gatherer bot completes one move-and-gather cycle (Priority: P1)

An Antithesis test is running on `cardano_node_master`. The admin bootstrap has deployed asteria's four validators, created the central Asteria UTxO at `(0,0)`, seeded a ring of fuel pellets, and pre-spawned N ships (one per gatherer replica). Antithesis schedules `parallel_driver_asteria_gatherer` replica `i`. That replica reads its assigned ship UTxO, finds the nearest pellet, submits up to 3 single-step `move_ship` transactions toward it, then one `gather_fuel` transaction. The ship's FUEL strictly increased, the ship's position now matches the pellet's. The replica emits one `reachable` SDK event on entry and one `always(ship_fuel_strictly_increased)` SDK event on success, and exits 0.

**Why this priority**: this is the minimum viable "real application" workload. If P1 does not run, P2/P3 have no system to observe.

**Independent Test**: can be fully tested by starting the testnet with one gatherer replica (N=1), confirming `docker compose exec sidecar /opt/antithesis/test/v1/asteria/parallel_driver_asteria_gatherer.sh` exits 0 after up to 3 moves and 1 gather on a healthy local cluster.

**Acceptance Scenarios**:

1. **Given** a healthy cluster with bootstrap complete and ship `i` at position `(sx, sy)` with `fuel = initial_fuel`, and a pellet at `(px, py)` with `fuel > 0` at Manhattan distance `d ≤ 3`, **When** replica `i` runs once, **Then** `d` move transactions land on-chain, one gather transaction lands on-chain, the ship UTxO's output datum has `pos = (px, py)`, the ship UTxO's output value has `FUEL` increased by the amount gathered, and the script exits 0.
2. **Given** the same setup but the nearest pellet is at distance `> 3`, **When** replica `i` runs once, **Then** the ship moves 3 steps toward the pellet, does not gather, and the script exits 0 with `sometimes(gatherFuel_not_attempted)` recorded.
3. **Given** a bootstrap that has not finished yet (no `ship_utxo_i.json` on the shared volume), **When** replica `i` runs, **Then** the script waits up to 30 s for the file, and if still missing, exits 0 with `sometimes(bootstrap_not_ready)` recorded.

---

### User Story 2 — Continuous invariants hold across the run (Priority: P1)

While gatherers run, an `anytime_asteria_invariants` command samples on-chain state periodically and asserts global invariants that must always hold: exactly one Asteria UTxO exists, the admin NFT balance at the asteria address is exactly 1, the global `ship_counter` read from the Asteria datum is monotonically non-decreasing since the last sample, and every live ship's `ship_fuel ≤ max_ship_fuel`. On any violation the script emits an `unreachable` SDK event and exits 1.

**Why this priority**: without continuous invariants, the "run" layer is dead weight. Antithesis's value is fault injection finding states invariants don't allow; invariants need to be declared.

**Independent Test**: tamper locally by creating a second Asteria UTxO by hand (out-of-band); `anytime_asteria_invariants.sh` must detect the duplicate and exit 1 with `unreachable("exactly_one_asteria_utxo")` in `sdk.jsonl`.

**Acceptance Scenarios**:

1. **Given** a healthy cluster, **When** the anytime command runs 10 times back-to-back, **Then** every run exits 0 and emits `always(exactly_one_asteria_utxo)` = true, `always(admin_nft_at_asteria)` = true, `always(ship_counter_monotone)` = true.
2. **Given** a state file showing `ship_counter = N`, **When** the anytime command reads on-chain `ship_counter = N - 1`, **Then** it emits `unreachable("ship_counter_monotone")` with details including both counters, writes new state `N - 1`, and exits 1.

---

### User Story 3 — End-of-run consistency is asserted (Priority: P1)

When drivers finish, Antithesis runs `finally_asteria_consistency`. It samples the final on-chain state and asserts: every token in the shipyard minting policy either has a live ship UTxO at the spacetime address OR a matching burn transaction in chain history; the Asteria pot's lovelace equals `ship_counter * ship_mint_lovelace_fee` if no Mines occurred, or reflects the partial drains if they did; no pellet has negative fuel. Emits one `always` per invariant.

**Why this priority**: post-workload final state is where "permanent fork" and "leaked state" bugs surface. `finally_` is the canonical place for those.

**Independent Test**: run `finally_asteria_consistency.sh` against a known-good local cluster after drivers have completed naturally; all three `always` assertions must pass. Tamper the admin NFT (simulate theft) and rerun; `always(admin_nft_conservation)` must fail.

**Acceptance Scenarios**:

1. **Given** a healthy final state after 30 gathers, **When** finally runs, **Then** all three `always(...)` assertions emit `condition=true` and the script exits 0.
2. **Given** a Kupo response missing one SHIP<n> whose history contains neither a Mine nor a Quit burn tx, **When** finally runs, **Then** it emits `always(shipyard_tokens_accounted)` with `condition=false` and exits 1.

---

### User Story 4 — Bootstrap is idempotent-on-restart (Priority: P2)

Antithesis may restart the bootstrap container mid-run (fault injection). If `serial_driver_asteria_bootstrap.sh` is invoked after the game is already live, it must detect the existing deployment (script-refs files present and their UTxOs on-chain, Asteria UTxO exists, at least one pellet exists) and exit 0 without re-minting or re-deploying.

**Why this priority**: non-idempotent bootstraps create duplicate Asteria UTxOs, duplicate ref-scripts, burn through admin NFTs. Worth getting right once; not P1 because for local validation we can manually ensure a clean start.

**Independent Test**: run bootstrap twice in a row against the same cluster; second run must exit 0 in under 5 s and the on-chain state is unchanged.

**Acceptance Scenarios**:

1. **Given** a cluster where bootstrap has completed once, **When** bootstrap is invoked again, **Then** it detects existing state, logs "already deployed", and exits 0.
2. **Given** a partial bootstrap (asteria ref-script exists but pellet ref-script is missing), **When** bootstrap is invoked, **Then** it resumes from the missing step without duplicating the asteria ref-script.

---

### User Story 5 — A deliberate contract regression is caught by SDK assertions (Priority: P3)

An on-chain bug is introduced in a forked asteria validator (for example: `gather_fuel` allows amount greater than the pellet's fuel). With the bug-bearing image deployed and agents running normally, one Antithesis duration=1h run produces a failing SDK assertion — either `always(pellet_fuel_non_negative)` falsifies (pellet below zero) or `sometimes(gatherFuel_matches_pellet_delta)` reveals a mismatch — proving the stack catches the bug class it was designed for.

**Why this priority**: proves the harness has teeth. Not P1 because it requires a forked asteria image and is validation rather than feature delivery.

**Independent Test**: ship a branch with a deliberately-relaxed validator, run Antithesis duration=1h, observe at least one `Always` finding with `condition=false` in the report and matching finding ID in the Properties section.

---

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: A new composer template directory `components/sidecar/composer/asteria/` MUST exist, containing:
  - `helper_sdk_lib.sh` (symlink to the existing `components/sidecar/composer/convergence/helper_sdk_lib.sh` or duplicate).
  - `serial_driver_asteria_bootstrap.sh`.
  - `parallel_driver_asteria_gatherer.sh`.
  - `anytime_asteria_invariants.sh`.
  - `finally_asteria_consistency.sh`.
- **FR-002**: A new component `components/asteria-bootstrap/` MUST produce a Docker image that, given `SEED`, `SHIPS`, `ANTITHESIS_OUTPUT_DIR`, and `CARDANO_NODE_SOCKET_PATH` / `OGMIOS_URL` / `KUPO_URL` env vars, performs the five-step admin deploy (mint admin NFT → deploy 3 ref-scripts → create Asteria UTxO → seed pellets → pre-spawn ships) and writes `/shared/asteria/script-refs/{asteria,pellet,spacetime}-ref.json` + `/shared/asteria/ships/ship_<i>.json` for `i in 0..SHIPS-1`.
- **FR-003**: A new component `components/asteria-gatherer/` MUST produce a Docker image exposing a single command that, given `SHIP_INDEX`, `SHIPS`, `KUPO_URL`, `OGMIOS_URL`, `ANTITHESIS_OUTPUT_DIR`, and the shared volume, performs one move-gather cycle and exits 0. The image MUST link a vendored asteria Blaze SDK with patches for the time-conversion and admin-token-from-env issues documented in #56.
- **FR-004**: `serial_driver_asteria_bootstrap.sh` MUST invoke the `asteria-bootstrap` image's command via `docker exec` (or the Antithesis composer equivalent) and emit `sdk_reachable("asteria_bootstrap_entered")` on entry, `sdk_sometimes(true, "asteria_bootstrap_completed")` on success, `sdk_unreachable("asteria_bootstrap_failed")` on any failure.
- **FR-005**: `parallel_driver_asteria_gatherer.sh` MUST read `SHIP_INDEX` from env or fall back to `$(hostname)` parsing, invoke the `asteria-gatherer` image's command, and emit: `sdk_reachable("asteria_gatherer_entered")`; `sdk_sometimes(moveShip_submitted, "gatherer_submitted_move")`; `sdk_sometimes(moveShip_accepted, "gatherer_move_accepted")`; `sdk_sometimes(gatherFuel_accepted, "gatherer_gather_accepted")`; `sdk_always(ship_fuel_strictly_increased, "gather_increased_fuel")` on success.
- **FR-006**: `anytime_asteria_invariants.sh` MUST read current on-chain state via Kupo + Ogmios + `cardano-cli`, carry forward a state file `/tmp/asteria-anytime-state.json` across invocations, and emit: `sdk_always(exactly_one_asteria_utxo)`, `sdk_always(admin_nft_at_asteria)`, `sdk_always(ship_counter_monotone)`, `sdk_always(all_ship_fuel_within_max)`. On any false, exit 1.
- **FR-007**: `finally_asteria_consistency.sh` MUST sample final on-chain state and emit: `sdk_always(shipyard_tokens_accounted)`, `sdk_always(asteria_pot_consistent)`, `sdk_always(all_pellet_fuel_non_negative)`. Total wall time MUST be under 60 s.
- **FR-008**: The existing sidecar image MUST grow to include any additional tools the new `helper_sdk_lib`-based scripts need beyond jq + coreutils + cardano-cli. If none are needed, the sidecar image stays unchanged.
- **FR-009**: `testnets/cardano_node_master/docker-compose.yaml` MUST add an `asteria-bootstrap` service depending on `relay1: service_started`, and `SHIPS` × `asteria-gatherer-<i>` services each depending on `asteria-bootstrap: service_completed_successfully`. Initial SHIPS value MUST be 3.
- **FR-010**: The vendored asteria fork (to be created at `components/asteria-vendor/` or similar) MUST include patches to: `sdk/blaze/src/utils.ts` (replace hardcoded `preview_knownTime` with an env-driven `KNOWN_TIME`/`KNOWN_SLOT` pair); `offchain/constants.ts` (admin token from env); reconcile the spacetime parameter mismatch (6 vs 8 params); clarify PlutusV2 vs PlutusV3 declaration.
- **FR-011**: All new composer commands MUST be executable locally via `docker compose exec sidecar /opt/antithesis/test/v1/asteria/<cmd>` and exit 0 on a healthy cluster, before any Antithesis run is triggered.
- **FR-012**: An Antithesis duration=1h run on this branch MUST return `findings_new` delta over the main baseline equal to 0 (ogmios-255 excluded per #49).

### Key Entities

- **AsteriaDeployment** — the bootstrap's output: `{asteria_ref: OutRef, pellet_ref: OutRef, spacetime_ref: OutRef, asteria_utxo: OutRef, admin_token: AssetClass, pellets: [Pellet], ships: [Ship]}`.
- **Pellet** — `{pos_x: Int, pos_y: Int, fuel: Int, utxo: OutRef}`.
- **Ship** — `{index: Int, pos_x: Int, pos_y: Int, ship_token_name: String, pilot_token_name: String, last_move_latest_time: Int, fuel: Int, utxo: OutRef}`.
- **GathererState** — per-replica persistent state across invocations: `{assigned_ship_index: Int, last_known_utxo: OutRef, last_action_slot: Int}`.
- **AnytimeState** — carried in `/tmp/asteria-anytime-state.json`: `{last_ship_counter: Int, last_check_slot: Int}`.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: A local `just up` followed by manual invocation of the four new composer commands completes successfully on a three-pool cluster within 90 s total wall time (bootstrap ≤ 60 s, one gather cycle ≤ 20 s, anytime + finally ≤ 10 s each).
- **SC-002**: An Antithesis duration=1h run on this branch returns `Completed` with `outcome: success` and `findings_new = 0` (excluding any unrelated #49-class findings).
- **SC-003**: The report's Properties section lists at least 8 new properties whose IDs begin with `asteria_*`, all passing.
- **SC-004**: Logs Explorer reports non-zero event counts for `source=asteria-bootstrap` (≥ 20) and `source=asteria-gatherer-*` (≥ 100 combined) across the duration=1h run.
- **SC-005**: Deliberately introducing a broken asteria validator (e.g. `GatherFuel(amount)` that does not burn FUEL on the pellet side) causes at least one `Always` finding on the resulting run, matching finding ID introduced by that branch.

## Assumptions

- Antithesis honors docker-compose `depends_on: service_completed_successfully` for the bootstrap step. (Confirmed in existing tests/cardano_node_master via `configurator: service_completed_successfully`.)
- The shared volume between `asteria-bootstrap` and `asteria-gatherer-*` can hold O(10 KB) of JSON without permission issues.
- The asteria Aiken validators compile against a Conway-era node. This testnet is already Conway-capable (nodes 10.5.3 / 10.6.2 / 10.7.1 run Conway).
- Blaze SDK's dependency on Ogmios and Kupo is acceptable. Since #49 may still block ogmios, the gatherer has a fallback path using only `cardano-cli` for submission and a lightweight direct ledger-state query for reads. If ogmios is unavailable, the gatherer still works — with lower fidelity.
- `duration=1h` runs complete in the post-fault-injection window without `eventually_*` commands racing. Per the skill's empirical data, 1h runs are duration-sensitive but this composer already passes at 1h on main (since PR #53).

## Out of Scope (Phase 2+)

- `mine_asteria` driver and the winner-takes-50%-of-pot end-of-round mechanics.
- `quit` driver.
- `add_new_ship` contention (serial bottleneck — one new ship per block).
- Plutus prize tokens attached to pellets.
- Leaderboard / history reconstruction via chain-walk.
- Alternative archetypes (arbitrageur, fuel-hoarder, racer).
- Hydra layer-2 via `MeshJS/asteria`.
- ogmios / kupo as first-class agent dependencies (blocked on #49).

## Open Questions & Risks

- **OQ-1**: How many ships should phase one support? Proposal: `SHIPS=3` to match `POOLS=3`. Revisit if parallel-driver scheduling weights warrant a different number.
- **OQ-2**: Vendor asteria as a git submodule, or as a pinned source-repository-package-style copy in `components/asteria-vendor/`? Submodule simplifies upstream tracking but complicates nix builds. Plan to decide in `/speckit.plan`.
- **RISK-1**: Time-conversion patch may be harder than it looks if the Blaze SDK's `slotToUnix` is called from multiple transitive dependencies. Mitigation: small wrapper that monkey-patches the constant via a TS barrel file. Fall back to a fork PR against upstream.
- **RISK-2**: Asteria's ref-script deployment needs three ordered txs before the game is usable. If Antithesis kills the bootstrap container mid-deploy, User Story 4 (idempotent bootstrap) must handle the partial state. This is already P2; if we can't get idempotence right, fail the run with a clear message rather than corrupting state.
- **RISK-3**: Spacetime parameter mismatch (6 vs 8) may reveal that asteria upstream is mid-refactor. If the Lucid builder is actually right and the plutus.json is stale, we'd need to rebuild the blueprint. Mitigation: compile asteria locally and compare.
