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

## State — PARKED 2026-08-21 14:38Z

**OPERATOR PAUSE ACTIVE since 14:29:22Z; full subtree parked at 14:38:42Z.**
The public state page was refreshed before worker shutdown, as required. The
cna#205 epic owner relayed one durable pause through each ownership edge and
confirmed cna#229+#231 parked at 14:35:12Z, amaru-bootstrap#95 at 14:36:54Z,
and t75/ab#79 at 14:38:09Z before appending its terminal `PARKED` event. All
epic wait shells are stopped. Already-started GitHub Actions were intentionally
left running; their conclusions are evidence to reconcile after resume, not
merge or release authority. Agents must not merge, push, dispatch the exact
fixture, fire production, build, start a child run, or begin new work. Release
requires an explicit operator resume through the milestone owner.

Controller fire-4, run
[32470212421](https://github.com/cardano-foundation/cardano-node-antithesis/actions/runs/32470212421),
proved cna#227/PR#228's atomic peer-snapshot proposal live: the candidate
updated both pins and the regenerated resolution record in one commit and the
old anchor failure did not recur. The run then failed closed one stage later:
it read the candidate Build check only three seconds after push and treated
“not reported yet” as failure. The same candidate also exposed an Amaru/Nix
packaging gap: upstream Amaru now requires git build metadata unavailable in a
Nix-fetched tree. No image handoff or Antithesis launch occurred.

The fire gate has two remaining fixes, both parked at hosted boundaries:

- cna#229 plus governed recut cna#231 share draft PR#230. The governed
  forward correction passed independent audit and exact head `a76330b` was
  pushed without rewriting the known-red history. Fresh hosted evidence is
  green on Daily Amaru dry-run, Build, unit, quality, docs, preview, and image
  publication; Compose smoke was the sole context left running at park. Its
  eventual conclusion has not been reconciled by an agent. The PR is unmerged.
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
  hosted run returned red while the pause propagated. The ticket owner parked
  clean after recording that red and an unfrozen forward-correction draft as
  resume context, without starting correction work. Hosted live proof and
  closed PR#93's byte-unchanged fixture remain required before merge.

The t75/amaru-bootstrap#75+#79 handoff repair has crossed its slice terminal.
Its independent repair-delta audit passed all 5 rows at set-point, final
`b7d835a` was proven to equal the audited candidate plus only its task stamp,
and PR#76 is pushed with every current hosted context green. The epic owner
accepted slice 4 and routed the lane into ab#79's remainder; only the live-PR
phase is held until #95 merges and the exact fixture is green. Fresh store
capacity at 12:35Z was
59,570,909,184 bytes (55.48 GiB), still above the 54 GiB one-lane bar.

Two unexplained tokenless composer texts were destroyed without execution
during an input-integrity recovery: the epic-pane text was cleared before a
safe durable pointer was delivered, and the wedged t75 client was restarted
with conversation continuity. The epic owner now keeps one rotating
owner-level wait instead of indefinite background monitor shells. A later
#95 START-delivery race was corrected append-only: the first durable START was
13:12:51Z; a second START was redundant, and a `Closes #94` brief typo was
corrected to `Closes #95` before any PR existed.

## Priority and convergence

0. While parked, perform no merge, new dispatch, fixture run, or production
   fire. On explicit operator resume, first reconcile any GitHub Actions that
   completed while agents were stopped.
1. Accept and merge cna#229+#231 on PR#230 and amaru-bootstrap#95, the latter
   only with closed PR#93's exact PR#90-shape fixture green and unweakened.
   Amaru-bootstrap#88 and #91 are already merged.
2. Execute the next lawful production fire only after both remaining fixes are
   merged; report its real terminal, not merely green PRs. A real launch ends
   the UTC day's firing.
3. In parallel, continue ab#79's remaining work while holding its live-PR phase
   for #95's merge. Then finish cna#212, #208, #207 remainder, and #206 in that
   order unless a new live fire exposes an earlier our-side blocker.
4. Accumulate seven consecutive unattended days and audit the outcomes against
   the published artifact.

## Live owner topology

| window / panes | owner | milestone-visible state |
|---|---|---|
| `amaru:4 cna-e205-reliable-daily` `%5195` | cna#205 epic owner, Claude Fable; runtime `/tmp/ms-cardano-node-antithesis-2/e-auto/` | PARKED 14:38:42Z; subtree confirmed, no waits armed; wake only on explicit operator resume through milestone owner |
| `amaru:5 amaru-bootstrap-e205-t75-daily-handoff` `%6759` | epic-owned t75 ticket lane | PARKED 14:38:09Z; slice 4 accepted/pushed/hosted-green, 13 child roots archived, ab#79 remainder durably mapped |
| `amaru:7 cardano-node-antithesis-e205-t229-check-wait` `%7073` | epic-owned #229+#231 lane | PARKED 14:35:12Z at clean exact head `a76330b`; Compose smoke left external, no merge |
| `amaru:2 amaru-bootstrap-e205-t95-carried-patch` `%7104` | epic-owned #95 lane | PARKED 14:36:54Z at clean exact head `fd6b100`; hosted red and unfrozen correction draft recorded for resume, descendants retired |
| `amaru:3 amaru-bootstrap-e205-t94-era-history` `%7102` | terminal #94 placement lane | COMPLETE packet-delivered; issue closed, local operator packet preserved, no upstream publication |

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
