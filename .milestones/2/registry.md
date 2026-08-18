# Contract registry — milestone 2

## Inherited from ms1 (verified against ms1 closing audit 2026-07-29; excerpts go into every epic brief)
| contract | parties | invariant | enforced |
|---|---|---|---|
| producer image | amaru-bootstrap publishes → cna compose pins | all compose refs one digest; digest has a validated run | CI-ENFORCED (cna#202/PR #203, fa39820): dynamic census, proven able to fail on 6 negative shapes |
| amaru CLI surface | pragma-org/amaru → amaru-bootstrap mocks/scripts | mocks ⊆ real binary surface | CI-enforced (amaru-bootstrap#70/PR #71), drift-red proven |
| amaru log stream | amaru stdout → tracer-sidecar scoring | fatal lines reach the scored property | merged (cna#194); positive control proven |
| compose/image entrypoint | cna compose command ↔ image Cmd/Entrypoint | pairing starts the sidecar | ENFORCED 2026-07-31 (cna#196/PR#209 merged 5336940): live-boundary check, seeded-mismatch RED proven |
| findings gate semantics | cna constitution ↔ observability PRs | new-finding ≠ revealed pre-existing failure | declared-property exemption (cna#198), operator-approved |
| upstream pin | pragma-org/amaru main → amaru-bootstrap flake | bare upstream, no forks | build gate (constitution P I), proven able to fail |
| global parameters | genesis → bundle → amaru env | AMARU_GLOBAL_* derived, not hand-authored | partial — amaru-bootstrap#36 OPEN (queued remnant) |

## ms2 contracts (every NONE is a scheduled incident until commissioned or waived)
| contract | parties | invariant | enforced |
|---|---|---|---|
| automation trigger | pragma-org/amaru main → daily pipeline | once per UTC day, if main changed, a run happens unattended; unchanged days observably no-op | PROVISIONAL — walking skeleton MERGED to main 311dfc1 (2026-08-01, GitHub-scheduled daily, no host cron); expendable parts carry tracked TODOs; full enforcement via hardening 75→208→207→206 |
| harness-interface coverage | pragma-org/amaru main ↔ harness (mocks, compose, sidecar schema) | harness covers current main's interface or ALARMS; drift never yields vacuous green | NONE — E-A deliverable; alarm must be proven able to fail (seeded drift) |
| run-report honesty | pipeline → per-property report → desk | missing/failed/partial run is loudly RED; verdicts complete; a DEAD SCHEDULE is loudly red | NONE — E-A deliverable (#206 absence watchdog); INCIDENT EVIDENCE 08-15→08-18: workflow evaluation died silently for 4 days, nothing alarmed — the watchdog must be external to the workflow it watches |
| declared-red ledger | triage → declared-reds registry → future runs | declared red stays declared until upstream fix verified; suppression only by recorded decision | NONE — E-C candidate, after E-A |
| external publication boundary | lane evidence packets → OPERATOR → external humans | agents NEVER publish outside the operator-designated working repos (no upstream issues/comments/PRs/replies); every new red becomes an operator-ready evidence packet ≤24h; publishing and all human relations are the operator alone | STANDING ORDER (operator 2026-08-06, after unauthorized #1102/#1104 filings in their name); enforced by: clause mandatory in every brief + desk audit of externally-visible artifacts at each acceptance |
| desk input integrity | desk pane ← host automations | decisions enter only as typed operator answers | mitigated by channel rule; machine-owner audit (NUDGER-INJECTION-4914) |
| trunk merge gate | GitHub main ruleset ↔ lane guard-merge discipline | required status checks effective on main before any unattended merge | PLATFORM-ENFORCED 2026-07-31 (operator-authorized): ruleset 20131742 active, no bypass — contexts Build, Run unit Tests, Check code quality, Compose smoke test, publish-images, build-docs (all verified universal on PRs #209/#211). A-001 guarded-merge discipline retained as belt-and-braces. WIRING PROVEN at PR#211 merge 2026-08-01: all six contexts reported required (captured in t210/epic-acceptance.md); #207 release precondition MET |
| peer-snapshot provisioning | amaru build.rs ↔ bootstrap staging ↔ pinned cardano-configurations | staged bytes ≡ recorded rule resolution for the pinned pair (committer-date rule extracted from build.rs; future-date edges anchored to bump-time evidence) | ENFORCED 2026-08-07 (ab#77/PR#78 merged): anchored equivalence check + loud-fail on missing/tampered files, negative controls proven |
