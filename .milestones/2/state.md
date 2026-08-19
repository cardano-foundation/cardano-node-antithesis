# M2 — Amaru tested routinely under fault injection

State — Updated: 2026-08-19
Legend: ✅ done · 🟡 active/next · ⏳ queued · ⛔ blocked · ❓ unknown

```mermaid
flowchart LR
    e196["✅ cna#196 entrypoint"] --> e210["✅ cna#210 skeleton"] --> e213["✅ cna#213 fail-closed identity"] --> e219["✅ cna#219 evaluation restore"]
    e219 --> e221["✅ cna#221 day propagation"] --> e223["✅ cna#223 manual production trigger + 1/day cap (cap PROVEN in fire-1)"]
    e223 --> e225["🟡 cna#225 candidate-sha fix — at rest, unaudited (PR#226 draft)"]
    p78["✅ ab#77/78 real peer snapshots"] --> t75
    t75["🟡 ab#75 handoff: slice-1 ✅ · ab#79 ⛔ architecture escalation parked"] --> e212["⏳ cna#212"] --> e208["⏳ cna#208"] --> e207["⏳ cna#207"] --> e206["⏳ cna#206"]
    e225 --> fires["⏳ consecutive clean scheduled fires (0 so far; 2 fires, 2 named reds, both fixed or in fix)"] --> audit["⏳ outcome audit"]
    unknown["❓ next 04:17Z fire runs unfixed candidate-sha path unless #225 lands first"] -.-> fires
```

🟡 PAUSED by operator 2026-08-19 16:07Z — all lanes at recorded rest points, resume is one word at the desk. App 4639090 live; no operator actions pending. Fires: schedule 04:27Z (state-machine red → fixed by #222), dispatch 14:01Z (candidate-sha red → #225 in fix). Zero real Antithesis runs consumed to date; every red named same-hour. Fix→fire loop proven at 4-minutes-to-named-red.
