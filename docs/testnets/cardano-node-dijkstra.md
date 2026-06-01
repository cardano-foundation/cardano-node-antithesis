# Cardano Node Dijkstra

A single-version Cardano testnet for exercising the experimental Dijkstra era transition.

## Overview

This testnet runs all Cardano nodes on cardano-node 11.0.1, the released version used here for the experimental Dijkstra/PV12 hard fork.

- **4 block producers**: p1, p2, p3, p4 — forge blocks in a ring topology (p1↔p2↔p3↔p4)
- **3 relay nodes**: relay1, relay2, relay3 — non-producing nodes connected to all producers

### Supporting services

| Service | Role |
|---------|------|
| **configurator** | Generates genesis files, node configs, and signing keys at startup |
| **tracer** | cardano-tracer daemon collecting structured logs from all nodes |
| **tracer-sidecar** | Processes tracer logs into Antithesis assertions (chain convergence, error detection) |

## Network topology

```
p1 (11.0.1) ←→ p2 (11.0.1) ←→ p3 (11.0.1) ←→ p4 (11.0.1) ←→ p1
   ↑                ↑               ↑               ↑
relay1 (11.0.1) ----+---------------+---------------+
relay2 (11.0.1) ----+---------------+---------------+
relay3 (11.0.1) ----+---------------+---------------+
```

Producers form a ring. Relays connect to all producers.

## Configuration

- `testnet.yaml`: Genesis parameters and node configuration. Conway and earlier hard forks activate at epoch 0. Dijkstra also activates at epoch 0 with genesis protocol major 12 and `TestDijkstraHardForkAtVersion: 12`, so the cluster boots directly into Dijkstra for the smoke-test window.
- `docker-compose.yaml`: Minimal 11.0.1-only topology with producers, relays, tracer, tracer-sidecar, and configurator.
- `relay-topology.json`: Shared topology for relay nodes (connects to all producers).
- `tracer-config.yaml`: cardano-tracer log forwarding configuration.

## Running locally

```bash
# Smoke-test the testnet
scripts/smoke-test.sh cardano_node_dijkstra 600

# Start the testnet
just up cardano_node_dijkstra

# Check container status
just ps cardano_node_dijkstra

# View tracer-sidecar logs (chain progress)
just logs tracer-sidecar cardano_node_dijkstra

# Stop and clean up
just down cardano_node_dijkstra
```

## Version strategy

Dijkstra support is intentionally kept single-version for this first testnet. All producers and relays use cardano-node 11.0.1, pinned by image digest, so the cluster isolates the experimental Dijkstra/PV12 transition without adding cross-version compatibility as a variable.
