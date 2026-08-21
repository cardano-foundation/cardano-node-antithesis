# M2 — Amaru tested routinely under fault injection

State — Updated: 2026-08-21

Legend: ✅ done · 🟡 active/next · ⏳ queued · ⛔ blocked · ❓ unknown

> ⛔ **Operator pause effective 2026-08-21T14:29:22Z.** All milestone
> workers are being parked at durable boundaries. Already-started GitHub
> Actions are not being cancelled and may finish while agents are paused.
> No merge, fixture dispatch, production fire, or new work will occur until
> the operator explicitly resumes all workers.

```mermaid
flowchart LR
    pause["⛔ operator pause<br/>2026-08-21 14:29Z"] --> resume["🟡 explicit operator resume"]
    ladder["✅ controller repair ladder"] --> atomic["✅ atomic peer-snapshot bump<br/>cna#227 / PR#228"]
    atomic --> fire4["✅ fire-4 reached a later boundary<br/>honest pre-launch red"]
    fire4 --> observe["⛔ parked at hosted boundary<br/>PR#230 head a76330b<br/>Compose smoke running externally"]
    fire4 --> gitinfo["✅ ab#88 / PR#89<br/>deterministic git identity"]
    gitinfo --> archives["✅ ab#91 / PR#92<br/>tar.zst consumption"]
    archives --> noroute["✅ ab#94 placement<br/>no stock route, packet preserved"]
    noroute --> patch95["⛔ parked at hosted boundary<br/>PR#96 head fd6b100<br/>Build Gate running externally"] --> fire5["⏳ both PRs merged<br/>exact fixture green"]
    observe --> fire5
    fire5 --> launch["⏳ first full image handoff<br/>and one-hour Antithesis launch"]
    launch --> streak["⏳ seven consecutive unattended days<br/>0/7"] --> audit["⏳ published-artifact outcome audit"]
    capacity["✅ store admission recovered<br/>realizing gates green; token released"] --> t75["✅ t75 slice 4<br/>audited, pushed, hosted green"]
    t75 --> handoff79["⛔ ab#79 remainder parked<br/>live PR held for #95 merge"] --> hardening["⏳ cna#212 → #208 → #207 remainder → #206"] --> streak
    unknown["❓ production layers beyond bootstrap validation<br/>remain unprobed"] -.-> launch
```

Fire-4, controller run
[32470212421](https://github.com/cardano-foundation/cardano-node-antithesis/actions/runs/32470212421),
live-proved the atomic peer-snapshot fix, then failed closed because candidate
CI was observed only three seconds after push. The subsequent fixes isolated
and repaired each later Amaru packaging boundary without an image handoff or
Antithesis spend. The milestone streak remains **0/7**.

At the pause boundary, the next production fire remains gated by two draft
pull requests:

- [PR#230](https://github.com/cardano-foundation/cardano-node-antithesis/pull/230)
  carries bounded candidate-check observation and complete fault labeling.
  The one-file hosted ShellCheck correction passed independent audit, and
  exact head `a76330b` is green on Daily Amaru dry-run, build, unit, quality,
  docs, preview, and image publication. Compose smoke is the only hosted
  context still running. The PR remains draft and unmerged.
- [amaru-bootstrap PR#96](https://github.com/lambdasistemi/amaru-bootstrap/pull/96)
  carries the SHA-bound, repository-versioned custom-network era-history patch
  with executable retirement. Final independent audit passed all eight
  blocking rows with no residuals. Exact squashed head `fd6b100` is pushed;
  documentation deploy is green and Build Gate is running. Hosted live proof
  and the byte-unchanged
  [PR#93 fixture](https://github.com/lambdasistemi/amaru-bootstrap/pull/93)
  remain required before merge.

Amaru-bootstrap#88/PR#89 and #91/PR#92 are merged as `80b71cc` and
`8e17e68`. Issue #94 proved no stock in-repo route and closed
packet-delivered; its operator packet remains local and nothing was published
upstream.

The t75 handoff slice remains accepted at `b7d835a`, independently audited,
pushed, and hosted-green. The ab#79 remainder is parked; its live-PR phase
remains held until #95 merges and the exact fixture is green.

The frozen seven-consecutive-day outcome cannot finish by the current
2026-08-28 due date because the first 2026-08-21 eligible attempt was red.
Preserve the acceptance test and reforecast the date after resume; never waive
the streak.
