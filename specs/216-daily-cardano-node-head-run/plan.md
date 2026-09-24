# Plan: daily three-hour Cardano Node HEAD run (#216)

Parent #214. Consumes #215's controller and `CandidateReceiptV1` contract unchanged.

## Outcome

Once per UTC day a scheduled workflow prepares a fresh `master` HEAD candidate
(#215 path), claims the day, pins the candidate into an immutable consumer
commit of this repository, submits one real three-hour, fault-injected
Antithesis run through the existing MOOG path, waits for its terminal outcome
and publishes a receipt correlating every identity.

## Invariants (IDs)

| ID | Holds when |
|---|---|
| I216-01 | Only a candidate that reached #215's `submit-candidate PREPARED` can be submitted; the same controller runs, only the transport differs. |
| I216-02 | Day claim is atomic and durable: at most one real submission attempt per UTC day, including after a failed or incomplete attempt, and under concurrent invocations. The claim is taken before any MOOG contact. |
| I216-03 | The real request is exactly 3 hours with fault injection enabled, against the exact consumer repository, directory and commit. |
| I216-04 | The consumer commit is immutable and auditable: the rendered HEAD topology lives in its own testnet directory at a commit reachable from a ref the controller created, never floating `main`. The mixed-version `cardano_node_master` profile is untouched. |
| I216-05 | Every failed prerequisite (publication, provenance, Compose validation, smoke, day claim, request construction) stops before submission, with a `FAILED` receipt naming the stage. |
| I216-06 | No automatic retry: nothing in the scheduled path, the workflow or the transport re-submits. |
| I216-07 | The terminal receipt records UTC day, upstream SHA, image tag and digest, consumer SHA, workflow run URL, MOOG request/test id, report URL and terminal outcome; no credential text. |
| I216-08 | Manual recovery exists and is documented; the #215 fake-submission manual mode keeps working. |

## Strategy (decided)

- **Day claim = ref creation.** The controller claims the day by creating a
  day-scoped ref in this repository with a non-force push (for example
  `refs/tags/daily-cardano-node-head/<YYYY-MM-DD>` pointing at the consumer
  commit). Creation either succeeds once or fails; an existing ref means the
  day is consumed. No issue comments.
- **Consumer commit.** The consumer commit adds the rendered all-HEAD topology
  as its own testnet directory (e.g. `testnets/cardano_node_head/`) on top of
  the exact `main` the scheduled run started from.
- **Submission reuses the existing MOOG path** in `cardano-node.yaml`
  (dispatch at the consumer commit with `test=<head dir>`, `duration=3`,
  `no-faults=false`) rather than a second MOOG client. Any change to
  `cardano-node.yaml` is limited to exposing the MOOG test id / report for
  correlation.
- **Validation run.** The repository-required one-hour real validation uses
  the same path with duration 1 under a claim namespace that cannot consume a
  production day. It requires explicit operator authorization at run time.

## Slices

One slice: S1 `daily-run` — tasks T2161–T2169 below.
