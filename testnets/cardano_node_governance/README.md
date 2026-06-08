# cardano_node_governance

Conway **governance** workload under Antithesis fault injection.

## Topology

| Service | Role | Faults | Notes |
|---|---|---|---|
| `p1`, `p2` | block producers | **injected** | cardano-node 11.0.1 |
| `relay1` | relay | excluded | stable query/submit endpoint |
| `gov-cli` | cardano-cli driver host | excluded | sleeps; Antithesis execs the governance drivers |
| `gov-configurator` | one-shot init | n/a | cardonnay genesis + governance assets |
| `tracer`, `tracer-sidecar`, `log-tailer`, `sidecar` | support | excluded | reused from master |

Two block producers run under fault injection; the single relay is kept
out of faults so the governance drivers always have a reachable node.

## Genesis + assets (cardonnay)

`gov-configurator` runs cardonnay's `conway_fast` generator in a
generation-only mode (no nodes started) to produce:

- Byron/Shelley/Alonzo/**Conway genesis with the constitutional
  committee seeded** (`committee.members`, `threshold 0.6`,
  `committeeMinSize 2`);
- the full `governance_data/` asset set — DRep keys + registration
  certs, vote-stake keys + reg + **vote-delegation** certs, CC cold/hot
  keys + **hot-key authorization** certs;
- the faucet (`genesis-utxo`) and pool cold keys.

These are distributed to the per-node config volumes and to the gov-cli
volumes (`gov-data`, `gov-state`).

## Governance operations (gov-cli drivers)

Each logical cardano-cli operation is a separate composer driver
(`components/gov-cli/composer/governance-{bash,python}/`):

1. `first_setup` — submits the CC hot-key authorizations, DRep
   registrations and vote-stake delegations, then waits one epoch.
2. `parallel_driver_create_action` — submits an InfoAction.
3. `parallel_driver_vote` — casts DRep + SPO + CC votes on a pending
   action.
4. `anytime_` / `eventually_` / `finally_` validators.

InfoActions never enact, so the create/vote workload is unbounded and
chain state never drifts — ideal under continuous fault injection.

The python drivers use the standalone `cardano-clusterlib` library; the
bash drivers call `cardano-cli` directly. Pick one with the gov-cli
image build arg `DRIVER_LANG=bash|python`.

## Validation status

Syntax-checked only in this environment (no cardano-cli / cardonnay /
node runtime available). Before a paid Antithesis run, validate in
order:

1. `docker compose build` (builds gov-configurator + gov-cli).
2. `docker compose up gov-configurator` — confirm genesis +
   `governance_data` land in the volumes.
3. `docker compose up p1 p2 relay1` — **confirm pool-only liveness**
   (the chain produces blocks with no BFT node; `cardano-cli query tip`
   advances). This is the #1 assumption to verify. Fallback if it
   stalls: add a genesis-delegate (BFT) producer.
4. `docker compose exec gov-cli /opt/antithesis/test/v1/governance/first_setup.sh`
   then the create/vote drivers; check `cardano-cli conway query
   gov-state` shows proposals with DRep/SPO/CC votes.
5. Wire the published image digests and run a 1h Antithesis validation.
