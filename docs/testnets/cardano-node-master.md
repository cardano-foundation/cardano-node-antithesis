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
| **sidecar** | Network health checks and Antithesis setup signal |
| **asteria-bootstrap** | One-shot. Deploys asteria validators and creates the initial game UTxO. See [Asteria Player](../components/asteria-player.md). |
| **asteria-player-1**, **asteria-player-2** | Long-running asteria game players. Drive realistic tx traffic (spendScript, mint, ref-inputs, validity bounds) through the cluster. |

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
