# Cardano Node Master

A mixed-version Cardano testnet for Antithesis fault-injection testing.

## Overview

This testnet exercises the node-to-node protocol across multiple cardano-node versions and ecosystem clients:

- **3 block producers**: p1 (10.5.3), p2 (10.6.2), p3 (10.7.1) — forge blocks in a ring topology (p1↔p2↔p3)
- **2 relay nodes**: relay1 (10.6.2), relay2 (10.7.1) — non-producing nodes connected to all producers
- **Oura**: Rust/Pallas N2N client connected to relay1 via TCP, exercising ChainSync and BlockFetch mini-protocols

### Supporting services

| Service | Role |
|---------|------|
| **configurator** | Generates genesis files, node configs, and signing keys at startup |
| **tracer** | cardano-tracer daemon collecting structured logs from all nodes |
| **tracer-sidecar** | Processes tracer logs into Antithesis assertions (chain convergence, error detection) |
| **sidecar** | Network health checks and Antithesis setup signal |

## Network topology

```
p1 (10.5.3) ←→ p2 (10.6.2) ←→ p3 (10.7.1) ←→ p1
   ↑                ↑               ↑
relay1 (10.6.2) ----+---------------+
relay2 (10.7.1) ----+---------------+
   ↑
oura ──TCP──→ relay1
```

Producers form a ring. Relays connect to all producers. Oura connects via node-to-node TCP to relay1.

## Configuration

- `testnet.yaml`: Genesis parameters (poolCount, networkMagic 42, epoch length, protocol version)
- `docker-compose.yaml`: Full topology definition with YAML anchors for producers and relays
- `relay-topology.json`: Shared topology for relay nodes (connects to all producers)
- `oura-daemon.toml`: Oura N2N configuration with custom chain parameters
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
