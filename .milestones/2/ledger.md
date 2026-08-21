# Milestone 2 — Amaru tested routinely under fault injection
Home: cardano-foundation/cardano-node-antithesis milestone #2 · ledger: `milestones` branch, depth 1.
Desk: tmux `amaru` win `ms2-amaru-routine` (amaru:1, %4914) · runtime /tmp/ms-cardano-node-antithesis-2/.
Standing directives (operator): NO EXTERNAL PUBLISHING by agents — ever (2026-08-06; see registry external-publication-boundary; operator owns all human relations; upstream replies operator-only); seat contract 87eade1 (Claude T.O. → Codex commit owner → Claude auditor; agy/qwen draft-only); no AI attribution; worktrees under /code.
Provider directive (operator 2026-08-20, supersedes all prior): CODEX-FIRST — Claude is the low provider. Seats: Codex T.O. → Grok commit owner → Codex auditor preferred; Claude only as recorded exception; existing Claude seats reseat to Codex at natural restpoints. qwen draft-only; agy revoked.
Doctrine: desk = ask/answer/sweep; merges AUTHORIZED, executed by owning lane; verticalize doctrine (operator 2026-07-31): walking skeleton first, ≤1-day trunk slices, expendable-now OK, invisible-now NOT.
OPERATOR GOAL (2026-08-19, standing): amaru daily runs on Antithesis RELIABLY — no infra bugs from us, bootstrap included. Sequencing: PR#221/#222 merged ✓ (day-propagation fix) → epic reseat to Claude ✓ → t75 resume (claude/grok) → #212 → #208 → #207 → #206 → consecutive clean fires.

## STATE 2026-08-21 06:13Z: first streak-eligible fire RED, honestly. Scheduled run 32447033481 opened real ab PR#86 for Amaru 8fdca45b and reached bootstrap checks, proving yesterday's candidate-sha/env and Rust-nightly fixes live. The Build Gate then stopped on `peer-snapshot-anchor-negative-control`: the unmutated anchored record was already RED because the unattended pin-only proposal changed `flake.lock` without atomically refreshing `nix/peer-snapshots/resolution.json`, the selected cardano-configurations revision, and snapshot bytes. No image handoff or Antithesis launch occurred; streak remains 0/7. This is a new cross-boundary automation contract, not a reason to weaken the working alarm. NEXT: epic owner receives NOTE-036 only after the 09:00Z Claude eligibility recheck and selects a bounded fix lane; peer-snapshot atomic bump integration precedes t75/#212/#208/#207/#206. The 2026-08-28 due date is now unreachable under the frozen seven-day test; desk recommendation is to preserve acceptance and reforecast. Prior state below:
- t210 (skeleton): draft PR#211 head d624dae CLEAN — owner acceptance + task
  stamps DONE, epic independent gate PASS (22 focused; immutable daily-amaru-v1,
  19 tests, 0 real launches; #202 census + #196 live-boundary included);
  5/6 required contexts PASS; SOLE PENDING = Compose smoke test
  (run 30645481231, job 91205904344).
  RESUME: verify exact-head Compose smoke PASS → epic finalization/acceptance
  → guarded-merge decision. No merge, no real MOOG launch occurred.
- t75 (daily handoff): PARKED clean at planning head a2edaa3, PR#76 draft,
  pair stopped, rejected test-only evidence archived.
- t196: DONE (PR#209 merged 5336940; cna#196 closed).
- NO real Antithesis run has been launched by ms2 yet; first = #207/skeleton
  acceptance under existing authorization (max 1 real run per UTC day).

## Outcome test — RATIFIED (operator, 2026-07-31)
Daily on-change: bump → image → repin → 1h run, unattended; interface
coverage or ALARM (never vacuous green); honest per-property reports; missing
run = loud red; reds filed upstream ≤24h (desk-assisted); declared reds never
suppressed. COMPLETE after 7-day unattended streak, by outcome audit.
Frozen in milestone #2 description. Typed-answer channel rule in force.

## Time-bounded Claude hold — IN FORCE through 2026-08-21T09:00:00Z
Machine order POINTER-1787292477-3581374 preserves Claude panes %5195 and
%6759 untouched through 10:00 BST. Mechanical audit found no Amaru-owned
scheduled/background wake source, so none required disarming. At the boundary
Claude is only eligible for reconsideration after provider, resource and role
gates; it does not start automatically. The Codex milestone desk alone is
active on the scheduled-fire sweep. No cold-build authority was used.

## Live lanes
| window | owner | state |
|---|---|---|
| amaru:2 cna-e205-reliable-daily (%5195) | cna#205 epic owner, Claude Fable, root /tmp/ms-cardano-node-antithesis-2/e-auto/ | HELD untouched until 09:00Z; NOTE-036 pending acknowledged delivery; last durable state rests after ab#85 merge |
| amaru:3 amaru-bootstrap-e205-t75-daily-handoff (%6759, %6894) | t75 ticket owner Claude + slice-4 Codex commit owner | both preserved; Claude owner held, Codex candidate parked awaiting a fresh independent auditor and an admitted cold gate; `/nix/store` available 58.74GB vs 66.57GB one-lane bar |

## Epic cna#205 (verticalized order, operator-ruled)
✅ entrypoint + walking skeleton + identity/evaluability/cap/transport fixes → ⛔ peer-snapshot atomic bump integration → 🟡 75/79 handoff hardening → 212 → 208 → 207 remainder → 206.
Registry flips pending merged seeded-RED evidence per child.

## Contracts — see registry.md. Platform trunk gate ENFORCED (ruleset
20131742, 6 universal contexts, no bypass; able-to-fail proof due at first
live merge). #207 platform precondition MET.

## Queue / parked
- QUEUED at release (operator ruling 2026-08-03): standalone ticket — cardano-node Antithesis schedule 4x/day → 1x/day (cron 5 1 * * *); ask drafted at /tmp/ms-cardano-node-antithesis-2/queued-ask-cardano-node-cadence.md; saves ~3 Antithesis hours/day; outside epic #205.
- DONE 2026-08-07: t-snap peer-snapshot fidelity landed in ab#77/PR#78. Its positive anchor and negative controls worked in production on 2026-08-21; the newly exposed gap is that the unattended bump path does not yet refresh the anchored bundle atomically.
- Operator-parked: llm-settings PR #53 review; host GPG unlock.
- Remnants after loop: cna#140, ab#36, ab#16; ⛔ ab#54 (lambdasistemi/amaru#9).
- WATCH pragma-org/amaru#1104.
- Incidents on record: NUDGER-INJECTION-4914 (clean); qwen driver stalls ×2 on
  t210 (diagnostics hashed cf4fe825…, pattern case to operator if recurs).
