# Testnets

## Available commands

```bash
just
```

```text
Available recipes:
    attack testnet='cardano_node_master'            # attack the network with adversarial chain sync clients
    check-convergence testnet='cardano_node_master' # check convergence of nodes in a testnet
    default                                         # just this help message
    down testnet='cardano_node_master'              # stop a testnet
    exec container testnet='cardano_node_master'    # exec into a container
    logs container testnet='cardano_node_master'    # view logs of a container
    ps testnet='cardano_node_master'                # list containers in a testnet
    restart testnet='cardano_node_master'           # restart a testnet
    up testnet='cardano_node_master'                # start a testnet
```

## Running locally

Run the testnet locally before submitting to Antithesis to verify containers start and the chain converges:

```bash
# Start
just up

# Verify all containers are running
just ps

# Watch chain progress
just logs tracer-sidecar

# Stop and clean up volumes
just down
```

The `justfile` wraps `docker compose` commands. All images must be available locally (built) or published to `ghcr.io`.

## Available testnets

- [cardano_node_master](cardano-node-master.md): Mixed-version testnet with producers, relays, and N2N ecosystem clients
- [cardano_amaru](cardano-amaru.md): cardano-node 10.7.1 testnet with an in-cluster `amaru-bootstrap-producer` and two relay-only Amaru nodes gated on the produced bundle
- `cardano_amaru_epoch240`: Antithesis-only `cardano_amaru` variant with 240-slot epochs for one-hour campaigns
