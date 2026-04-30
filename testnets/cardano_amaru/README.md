# Cardano Amaru Testnet

This testnet extends the `cardano_node_master` shape with an Amaru
bootstrap path:

- three `cardano-node` block producers pinned to the official
  `10.7.1-amd64` image digest;
- two relays, also pinned to the same node release;
- the published `amaru-bootstrap-producer` image from
  `lambdasistemi/amaru-bootstrap`;
- two Amaru nodes whose entrypoints wait until the producer has
  committed a complete `testnet_42` bootstrap bundle.

The producer image is pinned by full source commit SHA:

```text
ghcr.io/lambdasistemi/amaru-bootstrap-producer:83e2f7af6b915e805f4c231f0d5bfe4ad5fa14d6
```

The cardano-node release image is pinned by tag and digest:

```text
ghcr.io/intersectmbo/cardano-node:10.7.1-amd64@sha256:3275d357053d21f3220f74b0854fd584e1fe322dfa1bbb78effd760c3191d14c
```

## Runtime Flow

```text
configurator -> p1/p2/p3/relay1/relay2
                  |
                  v
             p1 ChainDB
                  |
                  v
          bootstrap-producer
                  |
                  v
           amaru-bundle volume
             |             |
             v             v
          amaru-1       amaru-2
```

`bootstrap-producer` mounts the producer node's ChainDB read-write at
`/cardano/state`. This is a cardano-node 10.7.1 consensus API
requirement: immutable chunk validation opens files with write
permissions. The producer contract remains immutable-only by behavior.

Each Amaru entrypoint copies the final bundle into its private state
volume before it execs `amaru run`, so the two Amaru nodes do not share
writable chain or ledger stores.

## Local Commands

Validate the Compose model:

```bash
INTERNAL_NETWORK=false docker compose -f testnets/cardano_amaru/docker-compose.yaml config
```

Run the standard node smoke test:

```bash
./scripts/smoke-test.sh cardano_amaru 600
```

The standard smoke test proves the cardano-node network, sidecar, and
tx-generator still start and converge. It does not force the bootstrap
producer to complete, because a fresh local testnet needs two complete
Conway epochs behind the immutable tip before the producer can write the
bundle.

For the bootstrap-specific proof, inspect:

```bash
docker compose -f testnets/cardano_amaru/docker-compose.yaml logs -f bootstrap-producer
docker compose -f testnets/cardano_amaru/docker-compose.yaml ps bootstrap-producer amaru-1 amaru-2
```

Expected completion sequence:

1. `bootstrap-producer` prints `wrote /srv/amaru/testnet_42` and exits
   `0`.
2. `amaru-1` and `amaru-2` copy the bundle into their private state
   volumes.
3. `amaru-1` and `amaru-2` start with `amaru run`.

## Compatibility Constraint

This stack intentionally targets cardano-node 10.7.1. The Amaru
bootstrap CBOR projection is release-sensitive, so a green compile
against a different ledger dependency set is not evidence that the
runtime bytes are compatible with this cluster.
