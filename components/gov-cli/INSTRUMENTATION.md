# Instrumentation — measuring perturbation coverage

How the gov-cli composer drivers feed signals to Antithesis, and how to
read those signals to know whether the test **covered enough
perturbation**. Applies to both driver sets (`governance-bash`,
`governance-python`); the bash names are used below.

## The coverage model

Antithesis scores a run mostly from the assertions the drivers emit. A
`Sometimes` assertion passes if its condition was true **at least once
across all timelines**; an `Always` assertion must hold on every
evaluation. So:

- You enumerate the interesting states you want the perturbation to
  drive the system into, and assert each one with a *condition-bearing*
  `Sometimes`.
- The report's **Sometimes assertions** section is the coverage
  scorecard: **green = that state was reached under fault injection;
  red = a gap** (the faults never created it, or the workload can't get
  there).

Anti-pattern: `sometimes(true, "x")` always passes and measures nothing
(it is really a `reachable`). Every `Sometimes` here carries a real
condition.

## Signal sink

All signals are append-only JSONL to `$ANTITHESIS_OUTPUT_DIR/sdk.jsonl`
(default `/tmp/sdk.jsonl`). `helper_sdk.sh` writes the shape; `helper_*`
files are ignored by the composer scheduler (shared libraries).

## Primitives (`helper_sdk.sh`)

| Call | Type | Meaning |
|---|---|---|
| `sdk_reachable <id>` | reachability | this line ran ≥ once |
| `sdk_unreachable <id>` | always:false | this line must never run (also absorbs out-of-scope failures) |
| `sdk_sometimes <cond> <id> [details]` | sometimes | coverage — true ≥ once across timelines |
| `sdk_always <cond> <id> [details]` | always | invariant — true on every evaluation |
| `sdk_run_signal_safe <id> <cmd…>` | — | absorb SIGKILL/SIGTERM-induced exits (137/143/255/124) so container-stop faults don't trip "Always: zero exit" |
| `sdk_setup_complete [details]` | lifecycle | tell Antithesis the system is healthy and faults may start |

The optional **`details`** JSON is attached to the report entry — use it
to surface distributions (vote counts, stall windows, slots).

## Scheduling levers (the file prefix)

| Prefix | When | Faults | Used here for |
|---|---|---|---|
| `first_` | once, before drivers | off | `first_setup` — committee + DRep bootstrap |
| `parallel_driver_` | concurrent, repeated | on | `create_action`, `vote` — the workload + coverage |
| `anytime_` | continuous | on | `chain_progress` (witness), `govstate_invariant` |
| `eventually_` | after faults stop | off | `votes_recorded` — recovery check |
| `finally_` | after drivers finish | off | `governance_summary` — end-of-run marker |

## What we emit, and what each tells you

### Perturbation witness — `anytime_chain_progress.sh`
gov-cli queries the fault-excluded relay1, but relay1's tip reflects
p1/p2, which are under faults. The probe samples the tip slot, waits
`CHAIN_PROBE_WINDOW` (default 20 s — at ~0.5 blocks/s a normal window
sees ~10 blocks, so zero blocks ⇒ a real halt), then samples again:

| Assertion | Meaning when green |
|---|---|
| `chain_stalled_under_fault` | faults halted block production at least once — **the run actually perturbed governance** |
| `chain_producing` | the chain also produced blocks — it recovers |
| `relay_reachable_under_fault` (Always) | relay1, being fault-excluded, kept answering |

If `chain_stalled_under_fault` never goes green, the faults were too
weak/short to disrupt governance — widen fault scope or lengthen the run.
The probe also publishes a verdict to `gov-data/state/chain_verdict` that
the create/vote drivers read.

### Governance progressed under perturbation — `create` / `vote`
| Assertion | Meaning when green |
|---|---|
| `gov_op_under_perturbation` | a create or vote landed while the chain was recently stalled — governance is resilient to the perturbation |

### Quorum / distribution coverage — `vote`
| Assertion | Meaning when green |
|---|---|
| `actions_live` | gov-state held at least one live InfoAction to vote on (details: count) |
| `action_voted_by_all_roles` | one action got DRep **and** SPO **and** CC votes (details: per-role counts) |
| `action_majority_reached` | an action crossed a majority of all eligible voters (details: total, majority) |
| `vote_decision_{yes,no,abstain}` | each decision was cast at least once |
| `vote_recorded_{drep,spo,cc}` | each voter role recorded a vote |
| `vote_transient_failure` | a vote tx failed (faucet-lock/stalled submit) but the action stays live and is retried — fault-induced, not a lifecycle event |

### Lifecycle — `anytime_govstate_invariant.sh` (stateless, from gov-state)
| Assertion | Meaning when green |
|---|---|
| `action_near_expiry` | an action reached its final epoch (`expiresAfter == currentEpoch`) — the run lasted long enough for actions to reach end-of-life; the ledger owns the lifecycle, we just observe it |

### Invariants — `anytime_govstate_invariant.sh`
| Assertion | Meaning |
|---|---|
| `govstate_well_formed` (Always) | gov-state always parses and exposes proposals |
| `committee_quorum_maintained` (Always) | authorized committee members stay ≥ `committeeMinSize` (2) under faults — killing producers must not drop the on-chain committee quorum |

### Recovery — `eventually_votes_recorded.sh`
Single-shot post-fault check: after faults stop, assert at least one
action carries DRep + SPO votes (cold-start guarded).

## Hypervisor-steered randomness (`helper_gov.sh`)

`antithesis_rng` / `rng_mod` wrap `antithesis_random` (fallback
`/dev/urandom`). Every choice routed through them is a dimension
Antithesis explores: the vote driver picks the **action**, the **voter**
(random DRep/SPO/CC), and the **decision** (yes/no/abstain) this way.
More RNG-driven choices ⇒ more state-space coverage.

## Reading the result

After a run, the report's *Sometimes* list is the checklist. The ones
that should be green for "enough perturbation":

1. `chain_stalled_under_fault` — faults bit.
2. `gov_op_under_perturbation` — governance progressed despite a stall.
3. `action_voted_by_all_roles`, `action_majority_reached` — full quorum
   reached.
4. `action_near_expiry` — actions reached end-of-life (run long enough).
5. `vote_decision_*` (all three) — decision space exercised.

Any of these red is a concrete coverage gap to close (longer duration,
wider fault scope, or more drivers). `Always` rows
(`committee_quorum_maintained`, `govstate_well_formed`,
`relay_reachable_under_fault`) going red is a real regression.
