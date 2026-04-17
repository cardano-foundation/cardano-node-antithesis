# Cardano Node Master

A mixed-version Cardano testnet for Antithesis fault-injection testing.

## Overview

This testnet exercises the node-to-node protocol across multiple cardano-node versions and ecosystem clients:

- **3 block producers**: p1 (10.5.3), p2 (10.6.2), p3 (10.7.1) — forge blocks with a ring topology
- **2 relay nodes**: relay1 (10.6.2), relay2 (10.7.1) — non-producing nodes connected to all producers
- **Oura**: a Rust/Pallas N2N client connected to relay1 via TCP, exercising ChainSync and BlockFetch mini-protocols

Supporting services: tracer (cardano-tracer), tracer-sidecar (log processing and Antithesis assertions), sidecar (network health checks), and configurator (genesis and config generation).
