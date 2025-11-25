
WARNING: This project is in early development and is not production-ready. Use at your own risk.

# Cardano Node test assets for Antithesis

This repository contains test assets and configurations for running Antithesis tests against Cardano Node implementations.

## Structure of the repository

- `testnets/`: Contains configurations for various Cardano test networks.
- `components/`: Contains reusable components for setting up test environments.

## Components

These are docker containers useful to set up the test environment for Antithesis. Dependign on their role, they can be simple wrappers around external executables as well as complex services that we think can be useful in an active or passive way to test Cardano Nodes.

- `adversary/`: A Node 2 Node client that behave maliciously to test the robustness of Cardano Node implementations.
- `configurator/`: A component that generates configuration files for Cardano Node instances based on test requirements.
- `config/`: A special components that let antithesis be configured via a container.
- `sidecar/`: A set of tests that Antithesis is performing to validate the network status.
- `tracer/`: A component that collects and analyzes trace logs from Cardano Node instances during tests.
- `tracer-sidecar/`: A sidecar service that runs alongside
## Testnets

Currently we provide and maintain only one testnet configuration, but we plan to expand this list in the future. Some old famoud testnets are broken in the old-broken directory for historical reference [link](https://github.com/cardano-foundation/cardano-node-antithesis/tree/main/old-broken)

- `cardano-node-master/`: A testnet that runs the latest Cardano Node from the latest published image. It's made of 5 producer nodes, a tracer, a tracer-sidecar and a sidecar. The testnet is configured to run with the latest Cardano Node version and is used to validate the basic functionality of Antithesis.
You can find the configuration files in the [testnets/cardano-node-master](https://github.com/cardano-foundation/cardano-node-antithesis/tree/main/testnets/cardano_node_master) directory.

