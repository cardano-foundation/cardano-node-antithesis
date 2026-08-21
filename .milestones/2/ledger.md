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

## State — 2026-08-21 11:33Z

Controller fire-4, run
[32470212421](https://github.com/cardano-foundation/cardano-node-antithesis/actions/runs/32470212421),
proved cna#227/PR#228's atomic peer-snapshot proposal live: the candidate
updated both pins and the regenerated resolution record in one commit and the
old anchor failure did not recur. The run then failed closed one stage later:
it read the candidate Build check only three seconds after push and treated
“not reported yet” as failure. The same candidate also exposed an Amaru/Nix
packaging gap: upstream Amaru now requires git build metadata unavailable in a
Nix-fetched tree. No image handoff or Antithesis launch occurred.

Two hosted-only fix campaigns are therefore the fire-5 gate:

- cna#229 plus governed recut cna#231 share draft PR#230. The first campaign
  found three blocking rows; the repair's final audit found one false-receipt
  fault-labeling row, so the epic owner refused ship-with-residual and recut it
  under #231. PR#230's current dry-run red is an intermediate campaign state,
  not acceptance.
- amaru-bootstrap#88 has draft PR#89 green in all 3 hosted contexts and fixture
  PR#90 reproducing fire-4's exact candidate shape on top of the fix. The
  fixture proves the git-info repair at the candidate shape: Amaru built and
  `amaru snapshot create` executed. It then failed the next contract because
  the current upstream produced 0 of the required 3 snapshot directories.
  This snapshot-create output drift is outside #88 and becomes iteration 6
  when #88 closes; PR#90 remains the unweakened evidence.

The t75/amaru-bootstrap#75+#79 handoff repair remains parked at commit
`c185c77`, unpushed, with its single repair bounce intact. A-EPIC-002 preserved
the 54 GiB one-lane admission bar and granted no early realization. After a
brief recovery to 53.90 GiB, capacity fell below the emergency threshold
again; the epic owner's fresh 11:41Z measurement was 49,352,531,968 bytes
(45.96 GiB). The 50 GiB machine stop is active, with zero realization anywhere
in the subtree. Hosted-only lanes continue; t75 wakes only on a later fresh
measurement at or above 54 GiB.

## Priority and convergence

1. Accept and merge the cna#229+#231 observation/fault-labeling fix and the
   amaru-bootstrap#88 deterministic git-info fix, preserving both fixture
   proofs. On #88 terminal, immediately file and route iteration 6 for the
   snapshot-create output drift.
2. Accept and merge iteration 6 only with PR#90's exact candidate fixture
   green and unweakened.
3. Execute the next lawful production fire only after all three gate fixes are
   merged; report its real terminal, not merely green PRs. A real launch ends
   the UTC day's firing.
4. Finish the validated-handoff arc ab#75/#79, then cna#212, #208, #207
   remainder, and #206 in that order unless a new live fire exposes an earlier
   our-side blocker.
5. Accumulate seven consecutive unattended days and audit the outcomes against
   the published artifact.

## Live owner topology

| window / panes | owner | milestone-visible state |
|---|---|---|
| `amaru:2 cna-e205-reliable-daily` `%5195` | cna#205 epic owner, Claude Fable; runtime `/tmp/ms-cardano-node-antithesis-2/e-auto/` | ACTIVE: supervises #229+#231 and #88, then routes snapshot-output iteration 6; owns the three-fix merge order and next fire |
| `amaru:3 amaru-bootstrap-e205-t75-daily-handoff` `%6759`, `%6894` | epic-owned t75 ticket lane | BLOCKED by active <50 GiB machine stop; repair bounce preserved, zero realization |
| `amaru:4 cardano-node-antithesis-e205-t229-check-wait` `%7073` plus campaign panes | epic-owned #229+#231 lane | ACTIVE: governed recut on PR#230; hosted/static envelope only |
| `amaru:5 amaru-bootstrap-e205-t88-git-info` `%7074` plus campaign panes | epic-owned #88 lane | ACTIVE at terminal: git-info proven at stock and candidate shapes; PR#90 names snapshot-create drift as the next layer |

## Completed path and queue

- Controller walking skeleton, day propagation, manual trigger/coded cap,
  transport/idempotence, Rust toolchain, and atomic peer-snapshot bundle fixes
  are merged. Each daily fire failed later than the previous one and remained
  fail-closed without Antithesis spend.
- Peer-snapshot fidelity ab#77/PR#78 and atomic unattended refresh
  cna#227/PR#228 are enforced; see `registry.md`.
- Outside epic #205, the Cardano-node Antithesis schedule reduction to once
  daily remains queued at release. Remnants after this loop: cna#140, ab#36,
  ab#16; ab#54 is blocked by lambdasistemi/amaru#9. Watch pragma-org/amaru#1104.
