# M2 — Amaru tested routinely under fault injection

State — Updated: 2026-08-20
Legend: ✅ done · 🟡 active/next · ⏳ queued · ⛔ blocked · ❓ unknown

```mermaid
flowchart LR
    fixes["✅ fix ladder merged: #213 identity → #219 evaluation → #221 day-prop → #223 trigger+cap → #225 candidate-sha+env → ab#85 toolchain"] --> app["✅ App 4639090 first production act: opened real bump PRs on ab"]
    app --> cap["✅ cap able-to-refuse PROVEN (same-head re-fire correctly blocked)"]
    cap --> fire3["🟡 2026-08-21 04:17Z scheduled fire — end-to-end verification, FIRST streak-eligible attempt (watch armed, absence-by-06:00Z = RED)"]
    fire3 --> streak["⏳ consecutive clean scheduled fires (0 so far; 5 fires, 5 distinct layers found+fixed)"] --> audit["⏳ outcome audit"]
    t75["🟡 ab#75/#79 handoff hardening (slices landing on PR#76)"] --> e212["⏳ #212"] --> e208["⏳ #208"] --> e207["⏳ #207 remainder (+ supersede-constituent follow-up)"] --> e206["⏳ #206 external watchdog"]
    unknown["❓ layers beyond bootstrap-checks — unprobed until tomorrow's fire"] -.-> fire3
```

Notes: 2026-08-20 was the fix→fire compression day — five defect layers (state machine, env propagation, candidate-sha, PATH/bare-env, Rust-nightly toolchain drift) all found by fires and fixed same-day across both repos. Zero real Antithesis launches yet; allowance untouched; every red named. Cross-org App identity works in production. Next unknown begins where bootstrap-checks ended.
