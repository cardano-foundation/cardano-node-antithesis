# Milestone 2 — Amaru tested routinely under fault injection

Home: cardano-foundation/cardano-node-antithesis milestone #2 (the operator calls this seat "amaru test M1"). Desk: tmux amaru-anti, window cardano-node-antithesis-ms2-amaru-routine, pane %267, Claude Opus 5.5, taken 2026-10-12. Desk journal: /tmp/ms-cardano-node-antithesis-2/STATUS.md. Lane runtime: /srv/lanes/cna-ms2/.

## Outcome test

On each UTC day when upstream Amaru changes, the unattended controller bumps it, builds and publishes the validated image, repins its exact digest, and launches a one-hour fault-enabled Antithesis run. Missing, failed and partial runs stay distinct; each new red gets an operator-ready evidence packet within 24 hours. Completion needs seven consecutive unattended successful days and an independent audit of the published artifact and every contract in registry.md. Streak 0/7.

## State and priority

1. Daily delivery: unblocked by amaru-bootstrap #141 (22dc4c25) and #144 (f948e4c7), merged 2026-10-12. Next check: the scheduled run around 04:30 UTC on 2026-10-13 must claim, pass Build Gate and launch the fault run.
2. Amaru fault recovery: the Oct 1 relay exits were the rewards-conservation panic, fixed upstream and now adopted. Whether the relays survive faults is the 2026-10-13 run's result.
3. Harness oracle defects (tracer defaultK 432 vs k 20, lag counted as fork depth, consensus_died not matched) remain open under #140; not scheduled.
4. Open milestone issues: #205 epic, #206 receipts and silence alarm, #207 automated repin and launch, #208 interface drift, #140 fork depth.

## Lanes

None live. Retired: consumer-ci-sonnet-20261001, amaru-fix-20261008, bootstrap-merge-20261012. The Sonnet 5.5 seat resumes with `cd /code/amaru && claude --dangerously-skip-permissions --model claude-sonnet-5-5 --resume 3e932903-31dd-428c-ba40-e1e8ea12b85f`; a new run needs a fresh root and brief.

## Parked decisions

- Contract "Amaru upstream vs bootstrap patch and CLI surface" is enforced NONE (registry.md); the durable fix (upstream the era-history input so no patch is carried) is not commissioned.
- Stale PRs amaru-bootstrap #142, #143 and CNA #245, #248, #249, #250.
- Ownership of the harness oracle correction (#140).
- Wiki M2-State stale since before 2026-09-25; no publication claimed.

Epic 214 (Cardano Node head daily, ~/.orch-runtime/cna-e214) is outside M2 and paused.
