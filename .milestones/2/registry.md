# M2 contract registry — reconciled 2026-09-25

Every NONE below is open, not a waiver. The desk owns compatibility and sequencing; it commissions implementation through an approved immediate owner.

| contract | parties | invariant | current enforcement / evidence |
|---|---|---|---|
| producer image identity | amaru-bootstrap publisher → CNA compose | all references use one validated immutable digest | cna#202/PR#203 census and negative controls; current test digest sha256:1987b3504a6c4ea9e71f56ba7168f48b6ccea068a3cd3d01129abe4d51a1be2e |
| Amaru CLI surface | upstream Amaru → bootstrap scripts/mocks | mocks are a subset of the real executable interface | bootstrap#70/PR#71 drift control; current-interface completeness still open under #208 |
| Amaru log stream | Amaru stdout → tracer-sidecar score | fatal lines reach a real failing property | PARTIAL: cna#194 positive control exists, but the pinned no-fatal matcher misses lifecycle.consensus_died observed in run de045c2e; Q-M2-018 |
| compose/image entrypoint | CNA compose → published image | the actual command and entrypoint start the sidecar | cna#196/PR#209 live boundary control |
| peer snapshot bundle | daily controller → bootstrap pins/record/staged bytes | an Amaru pin change refreshes the complete anchored bundle atomically | cna#227/PR#228 and resolver; today's scheduled run passed image resolution before consumer-checks |
| automation trigger and consumer checks | upstream Amaru main → daily controller → CNA PR CI | changed head enters an unattended governed path, then all required exact-head checks run without manual approval | FAILED: scheduled run 36094778166 stopped at consumer-checks; PR #248 workflows action_required/no jobs; Q-M2-017 |
| validated image handoff | bootstrap publisher → CNA compose/controller | published image tag, digest, provenance and checked PR head remain bound | PARTIAL: daily run resolved an exact digest, but consumer exact-head CI did not run; #207 remains open |
| harness interface coverage | current Amaru → mocks/compose/sidecars | current interface is covered or alarms, never vacuously green | NONE: #208 open; separate tracer oracle defects in Q-M2-018 |
| fault-recovery observation | relay fault effects → Amaru process/result | recoverable connection errors do not silently terminate a relay | NONE: current-Amaru fault-enabled run de045c2e shows both relays exit 1; no accepted repair; Q-M2-018 |
| fork-depth semantics | tracer host map + testnet k → fork finding | lag on one chain is not called persistent divergence, and k matches the testnet | NONE: pinned sidecar defaultK 432 versus testnet k 20; #140 open; Q-M2-018 |
| run report honesty | controller/provider → property report → desk | absent, pending, failed and partial remain distinct | PARTIAL: #229/#231/#234/#235/#239 landed; #206, #232/#233 and new property interpretation still open |
| successful no-op state | controller exit-zero → next scheduled run | successful outcomes are distinguishable from a failed attempt | NONE: PR #240 remains draft; no current active owner observed |
| missing daily receipt | GitHub schedule → outside watchdog | a dead schedule is detected outside the workflow it watches | NONE: #232/PR #233 remain unlanded |
| declared-red ledger | triage decisions → later reports | a known red stays declared until verified repair and recorded clearance | NONE: no accepted M2 ledger control |
| Cardano Node image freshness | image matrix → Antithesis freshness property | intended tested image is current | NONE: #236/PR #238 disposition pending separately |
| carried era-history patch | upstream Amaru ↔ bootstrap patch | patch applies at exact pin and retires only on equivalent upstream support | ENFORCED BUT BRITTLE: PR #127 merged exact head 8f78cc74 after local coherent 5a2a build gate and hosted old-pin checks; U2/U3/U4 remained open |
| external human publication | project/operator → upstream humans | agents prepare evidence; operator alone publishes externally | standing operator ruling; no automated check claimed |

The M2 desk cannot count a manual test as an unattended day. The fault-enabled and no-fault comparison runs used a test branch, so both leave the milestone streak at 0/7. The published story register/wiki is older than these observations and needs reconciliation.
