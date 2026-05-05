# Cardano Node Master

A mixed-version Cardano testnet for Antithesis fault-injection testing.

## Overview

This testnet exercises the node-to-node protocol across multiple cardano-node versions:

- **3 block producers**: p1 (10.5.3), p2 (10.6.2), p3 (10.7.1) — forge blocks in a ring topology (p1↔p2↔p3)
- **2 relay nodes**: relay1 (10.6.2), relay2 (10.7.1) — non-producing nodes connected to all producers

### Supporting services

| Service | Role |
|---------|------|
| **configurator** | Generates genesis files, node configs, and signing keys at startup |
| **tracer** | cardano-tracer daemon collecting structured logs from all nodes |
| **tracer-sidecar** | Processes tracer logs into Antithesis assertions (chain convergence, error detection) |
| **sidecar** | Network health checks, the Antithesis setup signal, and the host of the chain-sync `adversary` driver (see [Adversary](../components/adversary.md)). |
| **log-tailer** | Streams the per-pool node logs into Antithesis's log explorer for offline triage. |
| **asteria-game** | Single container that hosts the long-lived utxo-indexer plus three short-lived binaries (`asteria-bootstrap`, `asteria-game`, `asteria-invariant`) fired by composer scripts. See [Why asteria-game is here](#why-asteria-game-is-here) below and [Asteria Player](../components/asteria-player.md). |

## Why asteria-game is here

Antithesis can only find bugs that the system **actually exercises** in
the simulation. A cluster running idle — three producers minting empty
blocks — explores almost nothing of the node's tx-handling code:
mempool admission, script evaluation, ledger validation under
contention, fork resolution while inputs are being consumed. Faults
fired against an idle cluster mostly prove that the cluster restarts.

The asteria-game container is the **workload generator** that turns
master into a *busy* testnet. Without it, the cluster has no
realistic tx pressure. With it, the cluster sees the full envelope:

- **`parallel_driver_asteria_player.sh`** — fired concurrently by the
  composer many times per timeline. Each invocation reads the asteria
  UTxO from chain via the long-lived utxo-indexer, plans a move, and
  for player-1 builds + signs + submits a `spawnShip` Plutus
  transaction (consume asteria → mint a `SHIP*` token → write to the
  spacetime address → produce the next asteria UTxO with
  `ship_counter += 1`). Player-2 and player-3 run the same observe
  + plan path without acting, exercising the read side.
- **`serial_driver_asteria_bootstrap.sh`** — exclusive-access driver
  that idempotently re-deploys the validators and admin NFT each
  invocation. Re-runs detect the existing state via the indexer and
  skip the mint, exercising the "is already deployed" path under
  fault injection.
- **`anytime_asteria_admin_singleton.sh`** — periodic invariant
  probe. Asserts (via `sdk_always`) that exactly one `asteriaAdmin`
  NFT exists at the asteria spend address — the one-shot mint
  policy must hold under any combination of forks, kills, and
  partitions.
- **`finally_asteria_consistency.sh`** — end-of-run consistency
  snapshot. `ship_counter` on the asteria UTxO must equal the count
  of `SHIP*` tokens at the spacetime address.
- **`eventually_alive.sh`** / **`finally_alive.sh`** — short
  liveness probes against the indexer's `ready` endpoint.

What this gives Antithesis to score:

- **Tx-build pressure under faults**. The player driver builds, signs,
  and submits real Plutus transactions concurrently with fault
  injection. Validation: a 1h validation run on
  [PR #128][pr-128] saw 22,990 player invocations,
  2,599 successful spawn-ship transactions landing on chain, and 149
  observed fork switches during the run.
- **Plutus-script ledger paths**. The validators (asteria.spend,
  spacetime.spend, shipyard.mint) execute on every ship spawn —
  exercises ref-input handling, datum decoding, redeemer validation,
  exec-budget accounting.
- **Multi-version interop**. Producers p1/p2/p3 run three different
  cardano-node versions; the same Plutus tx must validate identically
  across versions or the cluster forks.
- **Invariants that survive faults**. `asteria_admin_singleton`
  (sdkAlways) and `asteria_state_consistent` (sdkSometimes) provide
  oracle properties Antithesis can falsify if a fault produces a
  divergent view, a double-mint, or a lost ship.

The container is the **load + oracle** of the test. Its image is
pinned by SHA in `docker-compose.yaml`; building it locally goes
through `components/asteria-game/`. The composer scripts that drive
it live alongside the binaries in `components/asteria-game/composer/stub/`
and are baked into the image at build time.

For the architectural detail of how the binaries integrate with
`cardano-node-clients` (provider, submitter, indexer, TxBuild DSL,
fee bisection), see [Asteria Player](../components/asteria-player.md).

## Network topology

```
p1 (10.5.3) ←→ p2 (10.6.2) ←→ p3 (10.7.1) ←→ p1
   ↑                ↑               ↑
relay1 (10.6.2) ----+---------------+
relay2 (10.7.1) ----+---------------+
```

Producers form a ring. Relays connect to all producers.

## Configuration

- `testnet.yaml`: Genesis parameters (poolCount, networkMagic 42, epoch length, protocol version)
- `docker-compose.yaml`: Full topology definition with YAML anchors for producers and relays
- `relay-topology.json`: Shared topology for relay nodes (connects to all producers)
- `tracer-config.yaml`: cardano-tracer log forwarding configuration

## Running locally

```bash
# Start the testnet
just up

# Check container status
just ps

# View tracer-sidecar logs (chain progress)
just logs tracer-sidecar

# Stop and clean up
just down
```

## Version strategy

Older node versions get fewer instances, newer versions get more. Relay nodes always run recent versions since they are the public-facing nodes that dApps and ecosystem tools connect to.
