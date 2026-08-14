# M2 — Amaru tested routinely under fault injection

State — Updated: 2026-08-14
Legend: ✅ done · 🟡 active/next · ⏳ queued · ⛔ blocked · ❓ unknown

```mermaid
flowchart LR
    e196["✅ cna#196 entrypoint live-check"] --> e210["✅ cna#210 walking skeleton on trunk"]
    p78["✅ ab#77/78 real peer snapshots"] --> e213
    e210 --> e213["✅ cna#213 controller fix — merged, fail-closed identity"]
    e213 --> t75["⏳ ab#75 daily handoff (re-brief)"]
    t75 --> e212["⏳ cna#212 smoke determinism"] --> e208["⏳ cna#208 interface preflight"]
    e208 --> e207["⏳ cna#207 repin+launch remainder"] --> e206["⏳ cna#206 receipts + silence watchdog"]
    e206 --> streak["⏳ 7-day unattended streak"] --> audit["⏳ outcome audit"]
    cad["⏳ cardano-node cadence 4x/day → 1x/day"] --> audit
    foreign["❓ PR#217 desk-less lane — territory overlap on daily workflow"] -.-> cad
```

🟡 Next action (operator): provision the dedicated least-privilege GitHub App on lambdasistemi/amaru-bootstrap; DAILY_AMARU_APP_ID + DAILY_AMARU_APP_PRIVATE_KEY into cna. Until then the daily fire is an ⛔ explicit named-credential RED — honest, zero spend. The App turns it green.

Notes: cna#213 merged 2026-08-14 (PR#218) — controller crash fixed; from the next 04:17Z fire, a missing App yields an explicit RED receipt naming the credentials. Zero real Antithesis runs consumed to date. Chain otherwise parked; resume order: t75 re-brief → cadence ticket → #212 → #208 → #207 → #206.
