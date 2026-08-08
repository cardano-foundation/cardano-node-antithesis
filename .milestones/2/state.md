# M2 — Amaru tested routinely under fault injection

State — Updated: 2026-08-08
Legend: ✅ done · 🟡 active/next · ⏳ queued · ⛔ blocked · ❓ unknown

```mermaid
flowchart LR
    e196["✅ cna#196 entrypoint live-check"] --> e210["✅ cna#210 walking skeleton on trunk"]
    p78["✅ ab#77/78 real peer snapshots"] --> e213
    e210 --> e213["⛔ cna#213 controller fix — awaits Q-003 operator App decision"]
    e213 --> t75["⏳ ab#75 daily handoff (re-brief)"]
    t75 --> e212["⏳ cna#212 smoke determinism"] --> e208["⏳ cna#208 interface preflight"]
    e208 --> e207["⏳ cna#207 repin+launch remainder"] --> e206["⏳ cna#206 receipts + silence watchdog"]
    e206 --> streak["⏳ 7-day unattended streak"] --> audit["⏳ outcome audit"]
    cad["⏳ cardano-node cadence 4x/day → 1x/day"] --> audit
    foreign["❓ PR#217 desk-less lane — territory overlap on daily workflow"] -.-> cad
```

🟡 Next action (operator): Q-003 — dedicated least-privilege GitHub App on lambdasistemi/amaru-bootstrap; DAILY_AMARU_APP_ID + DAILY_AMARU_APP_PRIVATE_KEY into cna. Everything queued unblocks from it.

Notes: daily fires 08-06/07/08 red as DECLARED (controller defect + missing App identity); zero real Antithesis runs consumed; receipts correctly ABSENCE-RED. Machine-wide pause since 2026-08-08 20:55Z; chain parked; resume order A-003 → cna#213 → ab#75 → cadence ticket.
