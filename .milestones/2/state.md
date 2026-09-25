# M2 current state — 2026-09-25

Outcome: OPEN. Unattended successful-day streak: 0/7. Latest scheduled Daily Amaru run 36094778166 failed at consumer-checks after opening PR #248, whose exact-head workflows are action_required with no jobs. Q-M2-017 is the project-owner decision for unattended token/check repair.

Bootstrap boundary for that same scheduled run: GREEN at PR #130 head ba3ee299a3f3bec71a8c220970ac26c30ab5a77b. Exact-head Build Gate and Live Bootstrap Producer passed in workflow 36094829890; publisher workflow 36096304682 checked out that head and pushed digest sha256:936b6501db894745c9be2b94f6ca56643eabf9e46986b240de17341761edf57b. Controller logs show bootstrap-check-observation polls=23 and advancement through image resolution. PR #130 remains open. Later upstream Amaru main 37982ba1d6e288bca6e0ebd0f29a855007a9f9ad is not covered by this daily run.

Current-Amaru fault-enabled Antithesis run de045c2ea19428f79a64106af14e41fd-61-11 completed 35/38 properties passing. Both Amaru relays exited 1; fork-depth also failed. Same-head and same-image no-fault run adf5b9c0171a5d034a42cf1cf93186de-61-11 completed 36/37 properties passing; the sole red expected the fault injector to start. Q-M2-018 requests bounded ownership for the fault-recovery and harness-oracle defects. Neither run proves unattended delivery or advances the streak.

Desk: amaru:1 pane %694, /tmp/ms-cardano-node-antithesis-2/. The former cna#205 owner pane %432 and amaru:2 window were absent at this readback. No immediate child launch is authorized by this snapshot. The M2-State wiki still reflects an older publication; no current wiki sync is claimed. Source of current facts: M2 STATUS.md through 2026-09-25T15:11:41Z, Q-M2-017, Q-M2-018, and live GitHub issue/run/PR readbacks.
