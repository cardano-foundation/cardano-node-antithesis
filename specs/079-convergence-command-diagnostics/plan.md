# Implementation Plan: Improve Convergence Command Diagnostics

**Branch**: `fix/improve-convergence-command-diagnostics` | **Date**: 2026-04-28 | **Spec**: `spec.md`
**Input**: Feature specification from `specs/079-convergence-command-diagnostics/spec.md`

## Status

**Completed**: Issue #79 opened and moved to WIP.  
**Current**: Implement shared sidecar probe diagnostics and smoke coverage.  
**Blockers**: None.

## Summary

Keep the current Antithesis Composer shell-command interface, but move repeated tip probing into a shared Bash helper that records structured per-node results. The helper uses Composer's `helper_` support-file prefix so it is ignored by command validation. Update convergence commands to classify DNS, connection, and tip-protocol probe failures separately from reachable tip divergence. Align sidecar setup readiness with convergence by requiring `cardano-cli ping --tip` before setup completion, and update smoke testing to run the sidecar convergence command.

## Technical Context

**Language/Version**: Bash with jq inside the sidecar image  
**Primary Dependencies**: `cardano-cli`, `jq`, coreutils  
**Storage**: N/A  
**Testing**: `scripts/smoke-test.sh cardano_node_master 120`, shell syntax checks, `docker compose config`  
**Target Platform**: Linux containers under Docker Compose and Antithesis  
**Project Type**: Antithesis test assets  
**Constraints**: Sidecar image is intentionally minimal; avoid adding dependencies for this slice.  
**Scale/Scope**: Three producer nodes in `cardano_node_master`

## Constitution Check

- Composer-first workload: unchanged; commands remain under `components/sidecar/composer/convergence/`.
- SDK instrumentation mandatory: improved with more precise failure details and classification.
- Short-running commands: retry budgets stay bounded; smoke test adds one bounded sidecar convergence call.
- Duration-robust: clearer post-fault/final assertions reduce ambiguity across scheduled timelines, and setup readiness now checks the same `--tip` protocol path used by convergence.
- Minimal sidecar image: no new runtime packages.
- Image tag hygiene: sidecar compose image must be bumped to a branch commit containing the component changes.

## Design Decision

Bash remains the right short-term command layer because Antithesis Composer discovers shell scripts directly and the current image already provides the needed tools. The risky part is not Bash itself, but repeated ad hoc parsing that drops failure evidence and readiness checks that do not match the convergence probe. This PR centralizes parsing and makes setup use the same `--tip` readiness path. If future work needs protocol-level diagnostics, timeouts beyond `cardano-cli` behavior, or richer retry policy, open a follow-up to replace the probe helper with a small compiled binary.

## Project Structure

```text
components/sidecar/composer/convergence/
├── helper_tip_probe_lib.sh
├── eventually_converged.sh
├── finally_tips_agree.sh
└── serial_driver_tip_agreement.sh

scripts/
└── smoke-test.sh

specs/079-convergence-command-diagnostics/
├── spec.md
├── plan.md
└── tasks.md
```

## Verification

- Bash syntax check on changed shell scripts.
- Composer filename prefix validation for sidecar command files.
- Docker Compose config parse.
- Local smoke test on `cardano_node_master`.
- PR `publish-images` and follow-on compose smoke test.
- Branch Antithesis duration=1h before merge, compared against current main baseline.
