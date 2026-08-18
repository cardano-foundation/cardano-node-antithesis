# M2 — Amaru tested routinely under fault injection

State — Updated: 2026-08-18
Legend: ✅ done · 🟡 active/next · ⏳ queued · ⛔ blocked · ❓ unknown

```mermaid
flowchart LR
    e196["✅ cna#196 entrypoint live-check"] --> e210["✅ cna#210 walking skeleton on trunk"]
    p78["✅ ab#77/78 real peer snapshots"] --> e213
    e210 --> e213["✅ cna#213 controller fix — merged"] --> e219["✅ cna#219 workflow-evaluation restore — merged 08-18 after 4 silent days"]
    e213 --> t75["⏳ ab#75 daily handoff (re-brief)"]
    t75 --> e212["⏳ cna#212 smoke determinism"] --> e208["⏳ cna#208 interface preflight"]
    e208 --> e207["⏳ cna#207 repin+launch remainder"] --> e206["⏳ cna#206 receipts + silence watchdog"]
    e206 --> streak["⏳ 7-day unattended streak"] --> audit["⏳ outcome audit"]
    cad["⏳ cardano-node cadence 4x/day → 1x/day"] --> audit
    foreign["❓ PR#217 desk-less lane — territory overlap on daily workflow"] -.-> cad
```

🟡 Next action (operator): provision the dedicated least-privilege GitHub App on lambdasistemi/amaru-bootstrap; DAILY_AMARU_APP_ID + DAILY_AMARU_APP_PRIVATE_KEY into cna. Until then the daily fire is an ⛔ explicit named-credential RED — honest, zero spend. The App turns it green.

Notes: INCIDENT 08-15→08-18 — #218 broke workflow evaluation; the daily schedule was silently dead 4 days (no fires, no receipts, no alarm); fixed by #219/PR#220 (merged 18:06Z). Lesson banked in registry: the absence watchdog (#206) must live OUTSIDE the workflow it watches. Next fire should produce the explicit named-credential RED — the App (operator item, day 12) turns it green. Zero real Antithesis runs consumed to date. Parked: t75, cadence, #212/#208/#207/#206.
