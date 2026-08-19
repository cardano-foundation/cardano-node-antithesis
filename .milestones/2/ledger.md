# Milestone 2 — Amaru tested routinely under fault injection
Home: cardano-foundation/cardano-node-antithesis milestone #2 · ledger: `milestones` branch, depth 1.
Desk: tmux `amaru` win `ms2-amaru-routine` (amaru:1, %4914) · runtime /tmp/ms-cardano-node-antithesis-2/.
Standing directives (operator): NO EXTERNAL PUBLISHING by agents — ever (2026-08-06; see registry external-publication-boundary; operator owns all human relations; upstream replies operator-only); seat contract 87eade1 (Claude T.O. → Codex commit owner → Claude auditor; agy/qwen draft-only); no AI attribution; worktrees under /code.
Provider directive (operator 2026-08-19, supersedes 2+2 norm): MINIMIZE GPT/Codex — default seats Claude T.O. → Grok commit owner → Claude auditor; codex only when claude AND grok unavailable, reported before spawning; epic owner reseated to Claude at post-merge restpoint. qwen draft-only; agy revoked.
Doctrine: desk = ask/answer/sweep; merges AUTHORIZED, executed by owning lane; verticalize doctrine (operator 2026-07-31): walking skeleton first, ≤1-day trunk slices, expendable-now OK, invisible-now NOT.
OPERATOR GOAL (2026-08-19, standing): amaru daily runs on Antithesis RELIABLY — no infra bugs from us, bootstrap included. Sequencing: PR#221/#222 merged ✓ (day-propagation fix) → epic reseat to Claude ✓ → t75 resume (claude/grok) → #212 → #208 → #207 → #206 → consecutive clean fires.

## STATE 2026-08-19 EOD-approaching: FIX→FIRE LOOP LIVE. Operator goal standing: reliable daily runs, bootstrap included. Today: PR#222 merged (day-propagation); epic reseated Claude (codex minimized, exceptions recorded); PR#224 merged (manual production trigger, code-enforced 1-real-run/UTC-day cap + supersede) — fire-1 same-day: named RED in 4 min (malformed-candidate-sha @ bootstrap-proposal, cap/supersede PROVEN) → cna#225 fix in final audit under NOTE-030 STANDING ITERATION AUTHORITY (epic merges daily-loop-surface fixes at acceptance+green, re-fires until a fire REACHES a real launch; same-step-twice reds escalate). t75: slice-1 accepted (PR#76), slice-2 ab#79 campaign-3 in repair (hosted-probe proof per A-EPIC-001 — NO credentials on host, user-identity pushes PERMANENTLY banned). Rulings today: A-EPIC-001, NOTE-028 proportionality, NOTE-029 partial reversal of schedule-only, NOTE-030. Next scheduled fire 08-20T04:17Z. Prior state below:
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

## CLAUDE-HOLD — still IN FORCE for session amaru
No Claude workers; release only via RELEASE file in /tmp/machine/pausa/.

## Live lanes (paused)
| window | owner | state |
|---|---|---|
| amaru:2 cna-e205-t210-daily-skeleton (%5195) | E-A epic owner e-auto, codex-raw xhigh, root /tmp/ms-cardano-node-antithesis-2/e-auto/ | epic cna#205; paused post-acceptance of t210 gate; resume fragment resume/e205.md |
| amaru:3 cna-e205-t210-skeleton-owner (3 panes: owner %5231, codex driver %5243, qwen navigator %5239) | epic-managed t210 | paused; PR#211 awaiting Compose smoke verdict |
| (t75 seats released) | — | parked at a2edaa3 until skeleton lands |

## Epic cna#205 (verticalized order, operator-ruled)
✅ 196 entrypoint → 🟡 210 walking skeleton (PR#211) → hardening: 75 → 208 → 207 remainder → 206.
Registry flips pending merged seeded-RED evidence per child.

## Contracts — see registry.md. Platform trunk gate ENFORCED (ruleset
20131742, 6 universal contexts, no bypass; able-to-fail proof due at first
live merge). #207 platform precondition MET.

## Queue / parked
- QUEUED at release (operator ruling 2026-08-03): standalone ticket — cardano-node Antithesis schedule 4x/day → 1x/day (cron 5 1 * * *); ask drafted at /tmp/ms-cardano-node-antithesis-2/queued-ask-cardano-node-cadence.md; saves ~3 Antithesis hours/day; outside epic #205.
- DISPATCHED 2026-08-06 (scoped operator override of OMNIA-PAUSA, on record in pausa trail): t-snap peer-snapshot fidelity lane — Claude T.O. %5665, Codex commit owner, Claude auditor; INTERNAL ONLY (no-external-publishing clause verbatim in brief); derives timestamp→rev rule from pinned amaru source, real pinned cardano-configurations contents replace empty placeholders; START pending verification.
- Operator-parked: llm-settings PR #53 review; host GPG unlock.
- Remnants after loop: cna#140, ab#36, ab#16; ⛔ ab#54 (lambdasistemi/amaru#9).
- WATCH pragma-org/amaru#1104.
- Incidents on record: NUDGER-INJECTION-4914 (clean); qwen driver stalls ×2 on
  t210 (diagnostics hashed cf4fe825…, pattern case to operator if recurs).
