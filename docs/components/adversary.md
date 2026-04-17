# Adversarial node for Cardano-Antithesis

The idea is to create an executable which:

- can be triggered by Antithesis
- behaves like a node but with unusual, potentially adversarial behaviours
- in order to test the "real" nodes in the cluster run by Antithesis

Cardano protocol details:

- Node to node protocol (we act like a node)
- Specifically chain sync mini-protocol (p.21 of [Network-spec][Netspec]), peer
  propagation protocol.

- As a downstream peer:
  - Unsual behaviours:
    * ask to sync to random intersection points (for example, blocks that might have been rolled back)
    * multiple random connect/follow/disconnect (maybe use Antithesis for randomness?)
- As an upstream peer:
  - serve incorrect headers/blocks/chains.

Overall approach:

1. Using tracer sidecar, get all intersection points from logs of nodes in cluster
2. Write these chain points into a file on disk
3. Adversarial node reads this file selects random points
4. Provide a command (to let Antithesis decide when to use it)
5. When command is called, select random point from file and do a chain sync

Implementation considerations:

- Make a Haskell executable
- Feasible to start from scratch; cardano-wallet code as reference
- Must have [ouroboros-network][Ouroboros] (and possibly ouroboros-consensus-cardano) as dependencies
- Might need external C libraries (libsodium)
- All this might make it necessary to use Nix to get all dependencies working


### Issue

To be controlled by antithesis we have to create an idling container with scripts at the special directory `/opt/antithesis/test/v1`

To test it we have to exec into the container and run the script `parallel_driver_flaky_chain_sync.sh` located at `/opt/antithesis/test/v1/chain-sync-client/`

The scripts are located in the `compose` directory of this adversary component.

## Build the image

### Nix

1. Install Nix package manager from https://nixos.org/download.html
2. Connect to the Cachix cache for faster builds
    ```bash
    nix shell nixpkgs#cachix -c cachix use paolino
    ```
2. Build the image
    ```bash
    nix build .#docker-image
    version=$(nix eval --raw .#version)
    docker load < ./result
    ```

### Non-nix

There is a Dockerfile available
1. Build the image
    ```bash
    version=$(cat ./version)
    docker build -t ghcr.io/cardano-foundation/cardano-node-antithesis/adversary:dev -f ./Dockerfile .
    ```

## Run the test script

1. Change the image tag in the compose to `dev`
    ```bash
    test=../../testnets/cardano_node_master/docker-compose.yml
    sed -i 's|ghcr.io/cardano-foundation/cardano-node-antithesis/adversary:.*|ghcr.io/cardano-foundation/cardano-node-antithesis/adversary:dev|' $test
    ```
2. Start the testnet
    ```bash
    docker compose -f $test up -d
    ```
3. Exec into the adversary container
    ```bash
    docker compose -f $test exec adversary /bin/bash
    ```
4. Run the script
    ```bash
    /opt/antithesis/test/v1/chain-sync-client/parallel_driver_flaky_chain_sync.sh
    ```


## See Also

- link(s) to more detailed project documents
- License: see LICENSE
- Contributing: see CONTRIBUTING.md
- Security: see SECURITY.md
- Other projects by [HAL][HAL]
- Other projects by the [Cardano Foundation][CF]
- About [Cardano][Cardano]

<!-- MARKDOWN LINKS & IMAGES -->

[Ouroboros]: https://github.com/IntersectMBO/ouroboros-network
[Netspec]: https://ouroboros-network.cardano.intersectmbo.org/pdfs/network-spec/network-spec.pdf
[HAL]: https://github.com/cardano-foundation/hal
[CF]: https://github.com/cardano-foundation
[Cardano]: https://cardano.org/
