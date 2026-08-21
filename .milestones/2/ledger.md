# Milestone 2 — Amaru tested routinely under fault injection

Home: cardano-foundation/cardano-node-antithesis milestone #2 · ledger:
`milestones` branch. Desk: tmux `amaru:1 ms2-amaru-routine` pane `%6722` ·
runtime `/tmp/ms-cardano-node-antithesis-2/`.

Standing operator boundary: agents never publish to external humans; the
operator owns upstream relations. The milestone desk performs asks, answers,
and sweeps only. It supervises cna#205 through the epic owner and never reaches
into ticket or slice lanes. `cold_build_authority=NONE`; provider alternation,
secrets, and role fences remain in force. Agy is revoked and qwen is
draft-only. The 2026-08-21 Claude release restored the matching preserved
epic-owner context without resetting the Codex milestone context.

## Outcome test — frozen

Once per UTC day, when Amaru main changed, the controller must bump Amaru,
build and publish its image, repin the digest, and launch a one-hour Antithesis
run without a human. The harness covers the current interface or alarms; it
may never pass vacuously. Reports are honest per property; missing, failed, or
partial runs are loud red. Completion requires seven consecutive unattended
days and an outcome audit against the published artifact. Current streak:
**0/7**. No M2 Antithesis launch has yet occurred.

The first streak-eligible 2026-08-21 run was red, so the frozen seven-day test
cannot finish before the current `2026-08-28T00:00:00Z` due date. Preserve the
acceptance test and reforecast the date; never waive the streak.

## State — 2026-08-21 12:44Z

Controller fire-4, run
[32470212421](https://github.com/cardano-foundation/cardano-node-antithesis/actions/runs/32470212421),
proved cna#227/PR#228's atomic peer-snapshot proposal live: the candidate
updated both pins and the regenerated resolution record in one commit and the
old anchor failure did not recur. The run then failed closed one stage later:
it read the candidate Build check only three seconds after push and treated
“not reported yet” as failure. The same candidate also exposed an Amaru/Nix
packaging gap: upstream Amaru now requires git build metadata unavailable in a
Nix-fetched tree. No image handoff or Antithesis launch occurred.

The fire gate now has two remaining active fixes:

- cna#229 plus governed recut cna#231 share draft PR#230. The first campaign
  found three blocking rows; the repair's final audit found one false-receipt
  fault-labeling row, so the epic owner refused ship-with-residual and recut it
  under #231. Submission 1 is now under independent audit on local candidate
  `7516653`; PR#230's current dry-run red remains an intermediate campaign
  state, not acceptance. Three same-seat transport restarts are recorded for
  review by the ticket owner at the slice boundary.
- amaru-bootstrap#88/PR#89 is accepted and merged as `80b71cc` (accepted head
  `c596d2a`), satisfying gate condition 2 with all 3 hosted checks green. Its
  exact candidate fixture proved the git-info bridge and exposed the next
  interface: upstream now emits tar.zst snapshot archives rather than the
  directories the producer counts. Amaru-bootstrap#91's submission-1 audit
  passed 6/6 rows with zero builds on `c85b2b3`; final commit `3c63faa` is now
  under an independent finalization-delta audit. Draft PR#92 still points at
  its planning head, so no hosted candidate or green PR#90 fixture is claimed.

The t75/amaru-bootstrap#75+#79 handoff repair has crossed its slice terminal.
Its independent repair-delta audit passed all 5 rows at set-point, final
`b7d835a` was proven to equal the audited candidate plus only its task stamp,
and PR#76 is pushed with every current hosted context green. The epic owner
accepted slice 4 and routed the lane into ab#79's remainder; only the live-PR
phase is held until #91 merges because its proposal cannot pass the required
bootstrap checks before that fix. Fresh store capacity at 12:35Z was
59,570,909,184 bytes (55.48 GiB), still above the 54 GiB one-lane bar.

Two unexplained tokenless composer texts were destroyed without execution
during an input-integrity recovery: the epic-pane text was cleared before a
safe durable pointer was delivered, and the wedged t75 client was restarted
with conversation continuity. The epic owner now keeps one rotating
owner-level wait instead of indefinite background monitor shells.

## Priority and convergence

1. Accept and merge cna#229+#231 on PR#230 and amaru-bootstrap#91, the latter
   only with PR#90's exact candidate fixture green and unweakened. The third
   gate condition, amaru-bootstrap#88, is already merged.
2. Execute the next lawful production fire only after both remaining fixes are
   merged; report its real terminal, not merely green PRs. A real launch ends
   the UTC day's firing.
3. In parallel, continue ab#79's remaining work while holding its live-PR phase
   for #91's merge. Then finish cna#212, #208, #207 remainder, and #206 in that
   order unless a new live fire exposes an earlier our-side blocker.
4. Accumulate seven consecutive unattended days and audit the outcomes against
   the published artifact.

## Live owner topology

| window / panes | owner | milestone-visible state |
|---|---|---|
| `amaru:2 cna-e205-reliable-daily` `%5195` | cna#205 epic owner, Claude Fable; runtime `/tmp/ms-cardano-node-antithesis-2/e-auto/` | ACTIVE: one rotating owner-level wait; remaining fire-gate fixes are #229+#231 and #91 |
| `amaru:3 amaru-bootstrap-e205-t75-daily-handoff` `%6759`, `%6894` | epic-owned t75 ticket lane | ACTIVE: slice 4 accepted/pushed/hosted-green; ab#79 remainder underway, live-PR phase held for #91 merge |
| `amaru:4 amaru-bootstrap-e205-t91-snapshot-format` `%7089`, `%7100` | epic-owned #91 lane | ACTIVE: implementation audit passed; finalization-delta audit active on `3c63faa`, exact PR#90 fixture still required |
| `amaru:5 cardano-node-antithesis-e205-t229-check-wait` `%7073`, `%7098` | epic-owned #229+#231 lane | ACTIVE: submission-1 independent audit on `7516653`; transport-choice review due at slice boundary |

## Completed path and queue

- Controller walking skeleton, day propagation, manual trigger/coded cap,
  transport/idempotence, Rust toolchain, and atomic peer-snapshot bundle fixes
  are merged. Each daily fire failed later than the previous one and remained
  fail-closed without Antithesis spend.
- Peer-snapshot fidelity ab#77/PR#78 and atomic unattended refresh
  cna#227/PR#228 are enforced; see `registry.md`.
- Deterministic Amaru source identity under Nix is enforced by
  amaru-bootstrap#88/PR#89; the exact candidate fixture proved the bridge.
- T75 slice 4 is accepted on PR#76 at `b7d835a`: 5/5 audit rows killed,
  exact task-stamp tree proof, and all current hosted contexts green.
- Outside epic #205, the Cardano-node Antithesis schedule reduction to once
  daily remains queued at release. Remnants after this loop: cna#140, ab#36,
  ab#16; ab#54 is blocked by lambdasistemi/amaru#9. Watch pragma-org/amaru#1104.
