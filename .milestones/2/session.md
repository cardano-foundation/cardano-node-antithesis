# tmux `amaru` — M2 resurrection — swept 2026-08-21 12:44Z

The milestone desk asks, answers, and sweeps. Technical work remains in its
epic-owned windows and is resumed only through the cna#205 epic owner. The
child-authored fragment copied verbatim to `resume/e205.md` has SHA-256
`7d6436e36df2a4c5e9c8182b56e7676a8786b3c5367a63877a608f0a0930bb85`.
No cold-build authority exists.

## `amaru:1 ms2-amaru-routine` — milestone desk singleton `%6722`

Working directory: `/code/moog`. Current family: Codex. Launch:

`codex-raw --dangerously-bypass-approvals-and-sandbox --dangerously-bypass-hook-trust`

Runtime: `/tmp/ms-cardano-node-antithesis-2/`. Resume by loading
`.milestones/2/resume/ms.md`. Supervise only the cna#205 epic owner `%5195`;
do not implement or address its descendants.

## `amaru:2 cna-e205-reliable-daily` — cna#205 epic owner `%5195`

Working directory: `/code/cardano-node-antithesis`. Launch:

`claude --dangerously-skip-permissions --model claude-fable-5`

Runtime: `/tmp/ms-cardano-node-antithesis-2/e-auto/`. Resume from
`.milestones/2/resume/e205.md`, then read the runtime STATUS and epic map.
Fire-gate condition ab#88 is merged. The remaining gate is cna#229+#231 plus
ab#91; both must land before one production fire.

## `amaru:3 amaru-bootstrap-e205-t75-daily-handoff` — ab#79 remainder lane

Ticket-owner pane `%6759`, working directory
`/code/amaru-bootstrap-issue-75`, launch:

`claude --dangerously-skip-permissions --model 'claude-opus-5[1m]' --effort high --continue`

Runtime: `/tmp/ms-cardano-node-antithesis-2/e-auto/t75-claude-to-01/`.
Commit-owner pane `%6894`, same working directory, launch:

`codex-raw --dangerously-bypass-approvals-and-sandbox -m gpt-5.6-sol -C /code/amaru-bootstrap-issue-75 -c model_reasoning_effort=high`

Slice 4 is accepted at exact pushed head `b7d835a`; its final audit passed 5/5,
the tree differs from the audited candidate only by the task stamp, and every
current hosted PR#76 context is green. The lane now owns ab#79's remainder.
Its live-PR phase is held until #91 merges. Resume only through the epic owner.

## `amaru:4 amaru-bootstrap-e205-t91-snapshot-format` — active epic lane `%7089`

Working directory: `/code/amaru-bootstrap`. Launch:

`codex-raw --dangerously-bypass-approvals-and-sandbox -m gpt-5.6-sol -C /code/amaru-bootstrap -c model_reasoning_effort=high`

Runtime: `/tmp/ms-cardano-node-antithesis-2/e-auto/t91-codex-to-01/`.
Amaru-bootstrap#91 is the active snapshot-archive-format adaptation under a
bounded producer-surface grant. It must turn closed PR#90's exact candidate
fixture green without weakening it. Submission-1 implementation audit passed
on `c85b2b3`. Finalization-delta auditor pane `%7100` works from
`/code/amaru-bootstrap-issue-91-audit-final`; launch:

`codex-raw --dangerously-bypass-approvals-and-sandbox -m gpt-5.6-sol -C /code/amaru-bootstrap-issue-91-audit-final -c model_reasoning_effort=high`

The audit is active on final `3c63faa`. No COMPLETE, push, or green exact
fixture is claimed yet. Resume only through the epic owner.

## `amaru:5 cardano-node-antithesis-e205-t229-check-wait` — active epic lane `%7073`

Working directory: `/code/cardano-node-antithesis`. Launch:

`codex-raw --dangerously-bypass-approvals-and-sandbox -m gpt-5.6-sol -C /code/cardano-node-antithesis -c model_reasoning_effort=high`

Runtime: `/tmp/ms-cardano-node-antithesis-2/e-auto/t229-codex-to-01/`.
The lane owns governed recut cna#231 on existing PR#230, inheriting the #229
seed candidate and frozen instruments read-only. Submission-1 independent
auditor pane `%7098` works from
`/code/cardano-node-antithesis-issue-231-audit-s1`; launch:

`codex-raw --dangerously-bypass-approvals-and-sandbox -m gpt-5.6-sol -C /code/cardano-node-antithesis-issue-231-audit-s1 -c model_reasoning_effort=high`

The audit is active on candidate `7516653`. Three same-seat owner transport
restarts are recorded for ticket-owner review at the slice boundary. Resume
only through the epic owner; do not restart #229's closed campaign.
