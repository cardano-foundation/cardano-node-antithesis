# Tasks: Improve Convergence Command Diagnostics

**Input**: `spec.md`, `plan.md`

## Phase 1: Shared Probe Diagnostics

- [X] T001 Add a shared tip probe helper in `components/sidecar/composer/convergence/helper_tip_probe_lib.sh`.
- [X] T002 Update `eventually_converged.sh` to use structured probe results, concrete failure reasons, and aggregate failure kinds.
- [X] T003 Update `finally_tips_agree.sh` to distinguish producer reachability from tip agreement.
- [X] T004 Update `serial_driver_tip_agreement.sh` to classify unreachable samples during fault injection.

## Phase 1.5: Setup Readiness

- [X] T012 Update `components/sidecar/sidecar.sh` setup readiness to require sidecar-originated `cardano-cli ping --tip` success.

## Phase 2: Local Coverage

- [X] T005 Extend `scripts/smoke-test.sh` to execute sidecar convergence from the sidecar container.
- [X] T006 Run shell syntax checks, compose config, and local smoke test.

## Phase 3: Image Wiring and Review

- [X] T007 Commit sidecar and smoke changes.
- [X] T008 Bump the `cardano_node_master` sidecar image tag to the sidecar-change commit.
- [X] T009 Validate sidecar composer files use accepted command or `helper_` prefixes.
- [ ] T010 Push branch, open PR linked to issue #79, and wait for PR checks.
- [ ] T011 Run a branch Antithesis duration=1h and compare findings against main before merge.
