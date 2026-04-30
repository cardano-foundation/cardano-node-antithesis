
WARNING: This project is in early development and is not production-ready. Use at your own risk.

# Cardano Node test assets for Antithesis

This repository contains test assets and configurations for running [Antithesis](https://antithesis.com/) fault-injection tests against Cardano Node implementations.

## Structure of the repository

- `testnets/`: Testnet configurations (docker-compose + genesis parameters).
- `components/`: Reusable Docker containers for the test environment.

## Components

Docker containers that set up and drive the Antithesis test environment. Depending on their role, they range from simple wrappers around external executables to complex services for actively or passively testing Cardano Nodes.

- `adversary/`: A node-to-node client that behaves maliciously to test the robustness of Cardano Node implementations.
- `configurator/`: Generates genesis files, node configuration, and signing keys for the testnet using the [testnet-generation-tool](https://github.com/cardano-foundation/testnet-generation-tool).
- `config/`: Antithesis platform configuration container.
- `sidecar/`: Network health checks and Antithesis assertions that validate testnet status.
- `tracer-sidecar/`: Processes structured node logs from cardano-tracer into Antithesis assertions (chain convergence, error detection).

## Testnets

Currently we provide and maintain two testnet configurations. Some old testnets are preserved in the [old-broken](https://github.com/cardano-foundation/cardano-node-antithesis/tree/main/old-broken) directory for historical reference.

- `cardano_node_master/`: A mixed-version testnet with 3 block producers (10.5.3, 10.6.2, 10.7.1) and 2 relay nodes (10.6.2, 10.7.1). Includes a tracer, tracer-sidecar, and sidecar for observability and assertions. The topology exercises node-to-node protocol diversity across supported Cardano Node versions. See [cardano-node-master](testnets/cardano-node-master.md) for details.
- `cardano_amaru/`: A cardano-node 10.7.1 testnet that runs the published `amaru-bootstrap-producer` image inside the cluster, copies the produced `testnet_42` bundle into per-Amaru-node state volumes, and starts two Amaru nodes only after the bundle is complete. See [cardano-amaru](testnets/cardano-amaru.md) for details.

## Image publishing

Component images are published to `ghcr.io/cardano-foundation/cardano-node-antithesis/` via the `publish-images` workflow. The docker-compose references images by commit hash (e.g., `configurator:aa43ea4`). The publish workflow:

1. Scans `docker-compose.yaml` for image references matching the registry prefix
2. Resolves each tag (commit hash) to a git revision
3. Checks out that revision and builds from `components/<name>/`
4. Pushes the image tagged with both the short hash and full commit hash

To add a new component: create `components/<name>/Dockerfile`, reference it in docker-compose as `ghcr.io/cardano-foundation/cardano-node-antithesis/<name>:<commit>`, and push. The publish workflow builds and pushes it automatically on PR or merge to main.

The `cardano_amaru` testnet consumes an external image instead:

```text
ghcr.io/lambdasistemi/amaru-bootstrap-producer:83e2f7af6b915e805f4c231f0d5bfe4ad5fa14d6
```

That image is produced by `lambdasistemi/amaru-bootstrap` and is pinned
by full commit SHA because the bootstrap CBOR projection targets a
specific cardano-node release.
