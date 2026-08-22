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

## State — ACTIVE 2026-08-22 12:34Z

**The operator explicitly authorized cleanup and resume at 09:54:57Z.** The
public pre-wake transition was published first. The cna#205 epic owner
acknowledged at 09:58:45Z, cleaned obsolete windows through its own ownership
edges, reconciled both exact-head hosted conclusions, and woke the two
critical-path ticket owners. Terminal #94, merged #91, and post-merge #229/#231
windows are retired with runtime roots archived; no worktree or runtime
evidence was deleted.

PR#230 exact accepted head `a76330b` satisfied all nine guard contexts and
merged as `64024b9`; issues #229 and #231 closed, so the cna fire-gate half is
met. PR#96 exact head `fd6b100` remains hosted-red, open, and non-rewritten.
Its first forward-correction campaign exhausted both submissions without
killing the two carried discovery-ordering properties. The epic owner ruled a
final in-lane re-cut, S-095-CI-02, whose semantic/behavioral proof must kill
both mutant classes by construction. Rejected candidate `80c2fcf` and all
frozen instruments are read-only inheritance. Closed PR#93 head `b52ca563`
remains byte-preserved. A hosted conclusion is evidence, never automatic merge
or release authority.

Controller fire-4, run
[32470212421](https://github.com/cardano-foundation/cardano-node-antithesis/actions/runs/32470212421),
proved cna#227/PR#228's atomic peer-snapshot proposal live: the candidate
updated both pins and the regenerated resolution record in one commit and the
old anchor failure did not recur. The run then failed closed one stage later:
it read the candidate Build check only three seconds after push and treated
“not reported yet” as failure. The same candidate also exposed an Amaru/Nix
packaging gap: upstream Amaru now requires git build metadata unavailable in a
Nix-fetched tree. No image handoff or Antithesis launch occurred.

The 2026-08-22 scheduled controller run
[32551716188](https://github.com/cardano-foundation/cardano-node-antithesis/actions/runs/32551716188)
ran on old cna main head `9094d54`, before PR#230 merged. It created a
candidate, then failed honestly because exact-head checks were not uniquely
successful: terminal `bootstrap-checks: exact-head-checks-failed`. No image was
published, no Antithesis run launched, and the streak remains 0/7.

The fire gate has one remaining fix, active at its hosted-red boundary:

- cna#229 plus governed recut cna#231 landed through PR#230. Exact accepted
  head `a76330b` passed all nine guards and merged as `64024b9`; both issues
  are closed and this half of the fire gate is complete.
- Amaru-bootstrap#88/PR#89 and #91/PR#92 are merged as `80b71cc` and `8e17e68`.
  #91 proved the tar.zst adaptation at its named boundary, then the unchanged
  PR#90-shape fixture, closed PR#93 head `b52ca563`, failed one layer later:
  upstream `amaru bootstrap` at `ba992f65` refuses custom networks before
  discovering the local archive. Issue #94 proved there is no stock in-repo
  route and closed packet-delivered. Desk ruling A-EPIC-003 authorized one
  explicit SHA-bound repository-versioned build-time patch. Issue #95's final
  independent repair audit passed all eight blocking rows with no residuals.
  The accepted tree plus task stamp was squashed to exact head `fd6b100` and
  pushed to draft PR#96. Documentation deploy was green; the already-started
  hosted run returned red while the pause propagated. The first
  forward-correction campaign exhausted both submissions with the same two
  discovery-ordering properties still open. Final re-cut S-095-CI-02 is now
  active under a frozen semantic/behavioral proof mandate. It must kill the
  different-snapshot-set and same-line-second-early-return mutant classes by
  construction; rejected `80c2fcf` and the frozen instruments are read-only.
  If either carried row survives, no third campaign is allowed and the carried
  patch returns to the milestone desk under A-EPIC-003. Hosted live proof and
  closed PR#93's byte-unchanged fixture remain required before merge.

The t75/amaru-bootstrap#75+#79 handoff repair has crossed its slice terminal.
Its independent repair-delta audit passed all 5 rows at set-point, final
`b7d835a` was proven to equal the audited candidate plus only its task stamp,
and PR#76 is pushed with every current hosted context green. The epic owner
accepted slice 4 and routed the lane into ab#79's remainder; only the live-PR
phase is held until #95 merges and the exact fixture is green. Fresh store
capacity at 12:35Z was
59,570,909,184 bytes (55.48 GiB), still above the 54 GiB one-lane bar.

Three unexplained tokenless composer texts have now been recorded. The first
epic-pane text was cleared before a safe durable pointer was delivered; the
wedged t75 client was restarted with conversation continuity; the third named
NOTE-077 before that file existed and was made harmless before the separately
token-bearing pointer was consumed. A subsequent NOTE-078 fragment receipt
falsely claimed an edit after its `sed` command failed. Fresh desk read-back
rejected the receipt; NOTE-079 landed the actual edit at fragment SHA-256
`92516650ee776c8adc12c9591c46de2ec91964a14a50d59bcd11561020f72ea9` and
removed the retired t231 wait. Fragment receipts now require mechanical
read-back. The epic owner keeps one owner-level wait on t95. The earlier #95
START-delivery race remains corrected append-only: first durable START
13:12:51Z; the second was redundant, and `Closes #94` became `Closes #95`
before any PR existed.

## Priority and convergence

0. DONE: obsolete windows cleaned and parked GitHub Actions reconciled.
   PR#230 merged as `64024b9`; no evidence or worktree was deleted.
1. Supervise final re-cut S-095-CI-02 to a semantic proof that kills both
   carried properties. If either remains open, stop the campaign ladder and
   escalate the carried-patch territory under A-EPIC-003. Merge
   amaru-bootstrap#95 only after a green accepted head and closed PR#93's exact
   PR#90-shape fixture are green and unweakened. Amaru-bootstrap#88 and #91 and
   cna#229+#231 are already merged.
2. Execute the next lawful production fire only after #95 is merged with its
   exact fixture green; report its real terminal, not merely a green PR. A real
   launch ends the UTC day's firing.
3. In parallel, continue ab#79's remaining work while holding its live-PR phase
   for #95's merge. Then finish cna#212, #208, #207 remainder, and #206 in that
   order unless a new live fire exposes an earlier our-side blocker.
4. Accumulate seven consecutive unattended days and audit the outcomes against
   the published artifact.

## Live owner topology

| window / panes | owner | milestone-visible state |
|---|---|---|
| `amaru:3 cna-e205-reliable-daily` `%5195` | cna#205 epic owner, Claude Fable; runtime `/tmp/ms-cardano-node-antithesis-2/e-auto/` | ACTIVE; PR#230 merged, supervising #95 with one foreground wait |
| `amaru:4 amaru-bootstrap-e205-t75-daily-handoff` `%6759` | epic-owned t75 ticket lane | PARKED/QUEUED: slice 4 accepted/pushed/hosted-green, 13 child roots archived, ab#79 remainder durably mapped |
| `amaru:2 amaru-bootstrap-e205-t95-carried-patch` `%7104` | epic-owned #95 lane | ACTIVE on final re-cut S-095-CI-02; PR#96 stays at non-rewritten red head `fd6b100`, with semantic proof and a no-third-campaign guard |

Retired during cleanup: #94 `%7102`, #91 `%7089`, and #229+#231 `%7073`;
their runtime roots are archived and their worktrees/evidence were not deleted.

## Completed path and queue

- Controller walking skeleton, day propagation, manual trigger/coded cap,
  transport/idempotence, Rust toolchain, and atomic peer-snapshot bundle fixes
  are merged. Each daily fire failed later than the previous one and remained
  fail-closed without Antithesis spend.
- Peer-snapshot fidelity ab#77/PR#78 and atomic unattended refresh
  cna#227/PR#228 are enforced; see `registry.md`.
- Deterministic Amaru source identity under Nix is enforced by
  amaru-bootstrap#88/PR#89; the exact candidate fixture proved the bridge.
- Tar.zst snapshot consumption is enforced by amaru-bootstrap#91/PR#92 at
  merge `8e17e68`; the fixture's later custom-network refusal is isolated to
  #94/#95 rather than attributed to #91.
- T75 slice 4 is accepted on PR#76 at `b7d835a`: 5/5 audit rows killed,
  exact task-stamp tree proof, and all current hosted contexts green.
- Outside epic #205, the Cardano-node Antithesis schedule reduction to once
  daily remains queued at release. Remnants after this loop: cna#140, ab#36,
  ab#16; ab#54 is blocked by lambdasistemi/amaru#9. Watch pragma-org/amaru#1104.
