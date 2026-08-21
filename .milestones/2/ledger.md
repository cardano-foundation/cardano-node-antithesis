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

## State — 2026-08-21 11:58Z

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
  under #231. PR#230's current dry-run red is an intermediate campaign state,
  not acceptance.
- amaru-bootstrap#88/PR#89 is accepted and merged as `80b71cc` (accepted head
  `c596d2a`), satisfying gate condition 2 with all 3 hosted checks green. Its
  exact candidate fixture proved the git-info bridge and exposed the next
  interface: upstream now emits tar.zst snapshot archives rather than the
  directories the producer counts. That bounded adaptation is active as
  amaru-bootstrap#91; its acceptance requires closed fixture PR#90's exact
  candidate shape to turn green unweakened.

The t75/amaru-bootstrap#75+#79 handoff repair is still unpushed at `c185c77`,
but it is no longer capacity-blocked. Three stable 11:47Z measurements reached
about 55.77 GiB, satisfying A-EPIC-002's 54 GiB admission bar. Both realizing
legs then passed (`slice-04-v3` plus `just build-gate`), the build token was
released, and 59,619,618,816 bytes (55.52 GiB) remained free. The final
independent repair-delta Claude auditor is verified active in pane `%7091`;
pass advances to stamp/push, while findings close the campaign under
A-007/A-008.

## Priority and convergence

1. Accept and merge cna#229+#231 on PR#230 and amaru-bootstrap#91, the latter
   only with PR#90's exact candidate fixture green and unweakened. The third
   gate condition, amaru-bootstrap#88, is already merged.
2. Execute the next lawful production fire only after both remaining fixes are
   merged; report its real terminal, not merely green PRs. A real launch ends
   the UTC day's firing.
3. In parallel, take ab#75/#79 through its final independent audit and lawful
   stamp/push. Then finish cna#212, #208, #207
   remainder, and #206 in that order unless a new live fire exposes an earlier
   our-side blocker.
4. Accumulate seven consecutive unattended days and audit the outcomes against
   the published artifact.

## Live owner topology

| window / panes | owner | milestone-visible state |
|---|---|---|
| `amaru:2 cna-e205-reliable-daily` `%5195` | cna#205 epic owner, Claude Fable; runtime `/tmp/ms-cardano-node-antithesis-2/e-auto/` | ACTIVE: #88 merged; supervises remaining fire-gate fixes #229+#231 and #91 plus t75 final audit |
| `amaru:3 amaru-bootstrap-e205-t75-daily-handoff` `%6759`, `%6894`, `%7091` | epic-owned t75 ticket lane | ACTIVE: realizing legs green and token released; final independent repair-delta audit verified active |
| `amaru:4 amaru-bootstrap-e205-t91-snapshot-format` `%7089` plus campaign panes | epic-owned #91 lane | ACTIVE: tar.zst producer adaptation, zero-realization envelope, exact PR#90 fixture required |
| `amaru:5 cardano-node-antithesis-e205-t229-check-wait` `%7073`, owner `%7094` | epic-owned #229+#231 lane | ACTIVE: governed fault-labeling recut on PR#230; same Claude conversation recovered through two transport restarts, submission 0/2 |

## Completed path and queue

- Controller walking skeleton, day propagation, manual trigger/coded cap,
  transport/idempotence, Rust toolchain, and atomic peer-snapshot bundle fixes
  are merged. Each daily fire failed later than the previous one and remained
  fail-closed without Antithesis spend.
- Peer-snapshot fidelity ab#77/PR#78 and atomic unattended refresh
  cna#227/PR#228 are enforced; see `registry.md`.
- Deterministic Amaru source identity under Nix is enforced by
  amaru-bootstrap#88/PR#89; the exact candidate fixture proved the bridge.
- Outside epic #205, the Cardano-node Antithesis schedule reduction to once
  daily remains queued at release. Remnants after this loop: cna#140, ab#36,
  ab#16; ab#54 is blocked by lambdasistemi/amaru#9. Watch pragma-org/amaru#1104.
