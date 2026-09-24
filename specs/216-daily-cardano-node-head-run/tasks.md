# Tasks (#216)

## S1 — daily-run

- [ ] T2161 Daily mode of the controller: candidate preparation unchanged, then day claim, consumer commit, real submission, wait, terminal receipt (I216-01..07).
- [ ] T2162 Atomic ref-creation day claim with duplicate, concurrent and after-failure refusal before MOOG (I216-02, I216-06).
- [ ] T2163 Consumer commit with the rendered topology in its own testnet directory at an immutable ref (I216-04).
- [ ] T2164 Real submission through the existing `cardano-node.yaml` MOOG path, 3 h, faults on; correlation of MOOG test id and report (I216-03, I216-07).
- [ ] T2165 Hermetic fake-transport suite with the named scenarios in the gate, wired into `just ci`.
- [ ] T2166 Scheduled workflow (once per UTC day) plus manual recovery entrypoint; #215 manual fake mode preserved (I216-08).
- [ ] T2167 One-hour real validation mode that cannot consume a production day claim.
- [ ] T2168 Operator docs: schedule, failure semantics, receipt lookup, manual recovery.
