# tmux `amaru` — M2 resurrection — swept 2026-08-21 12:12Z

The milestone desk asks, answers, and sweeps. Technical work remains in its
epic-owned windows and is resumed only through the cna#205 epic owner. The
child-authored fragment copied verbatim to `resume/e205.md` has SHA-256
`46f41c0354535848f63aa1bf5db904717dccd117403e297cf6a9b29aec62271b`.
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

## `amaru:3 amaru-bootstrap-e205-t75-daily-handoff` — final-audit epic lane

Ticket-owner pane `%6759`, working directory
`/code/amaru-bootstrap-issue-75`, launch:

`claude --dangerously-skip-permissions --model 'claude-opus-5[1m]' --effort high`

Runtime: `/tmp/ms-cardano-node-antithesis-2/e-auto/t75-claude-to-01/`.
Commit-owner pane `%6894`, same working directory, launch:

`codex-raw --dangerously-bypass-approvals-and-sandbox -m gpt-5.6-sol -C /code/amaru-bootstrap-issue-75 -c model_reasoning_effort=high`

The two realizing gates passed on local repair `c185c77`, and the build token
was released. Final repair-delta auditor pane `%7091` works from
`/code/amaru-bootstrap-audit-s8`; launch:

`claude --dangerously-skip-permissions --model 'claude-opus-5[1m]' --effort high`

The auditor's post-cursor verification landed at 11:58:10Z and its audit is
active. Resume only through the epic owner; pass advances to stamp/push,
findings close the campaign under its standing terms.

## `amaru:4 amaru-bootstrap-e205-t91-snapshot-format` — active epic lane `%7089`

Working directory: `/code/amaru-bootstrap`. Launch:

`codex-raw --dangerously-bypass-approvals-and-sandbox -m gpt-5.6-sol -C /code/amaru-bootstrap -c model_reasoning_effort=high`

Runtime: `/tmp/ms-cardano-node-antithesis-2/e-auto/t91-codex-to-01/`.
Amaru-bootstrap#91 is the active snapshot-archive-format adaptation under a
bounded producer-surface grant. It must turn closed PR#90's exact candidate
fixture green without weakening it. Current owner pane `%7092` works from
`/code/amaru-bootstrap-issue-91`; launch:

`grok --always-approve -m grok-4.6`

Resume only through the epic owner.

## `amaru:5 cardano-node-antithesis-e205-t229-check-wait` — active epic lane `%7073`

Working directory: `/code/cardano-node-antithesis`. Launch:

`codex-raw --dangerously-bypass-approvals-and-sandbox -m gpt-5.6-sol -C /code/cardano-node-antithesis -c model_reasoning_effort=high`

Runtime: `/tmp/ms-cardano-node-antithesis-2/e-auto/t229-codex-to-01/`.
The lane now owns governed recut cna#231 on existing PR#230, inheriting the
#229 seed candidate and frozen instruments read-only. Its same Claude
conversation survived two clean transport restarts and currently runs in pane
`%7094` from `/code/cardano-node-antithesis-issue-229` via `claude --continue
-p ... --dangerously-skip-permissions --model 'claude-opus-5[1m]' --effort
high`, still at submission 0/2 with unchanged authority. Resume only through
the epic owner; do not restart #229's closed campaign.
