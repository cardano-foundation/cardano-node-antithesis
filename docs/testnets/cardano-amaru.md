# Cardano Amaru

`cardano_amaru` is the first Antithesis testnet that starts relay-only
Amaru nodes from a bootstrap bundle produced inside the cluster.

The integration deliberately pins the node release. The
`amaru-bootstrap-producer` image used here emits an Amaru bootstrap
projection for cardano-node 10.7.1 ledger state, so every cardano-node
service in this testnet uses:

```text
ghcr.io/intersectmbo/cardano-node:10.7.1-amd64@sha256:3275d357053d21f3220f74b0854fd584e1fe322dfa1bbb78effd760c3191d14c
```

The producer image is pinned to the `amaru-bootstrap` commit that passed
CI and published the runtime image:

```text
ghcr.io/lambdasistemi/amaru-bootstrap-producer:d81dd7d31e1c23b3223d3c4155294b82dc56ea0e
```

## Stake Roles

Amaru is relay-only in this testnet. It is not assigned stake, and it is
not given producer credentials.

The only stake-bearing block producers are:

- `p1`
- `p2`
- `p3`

The Amaru services receive no KES key, VRF key, cold key, operational
certificate, or stake-pool genesis assignment. They start with
`amaru run` and an upstream peer only.

## Fast Bootstrap Profile

This testnet intentionally shortens the epoch/security window:

```yaml
protocolConsts:
  k: 10
epochLength: 120
securityParam: 10
activeSlotsCoeff: 0.2
TestConwayHardForkAtEpoch: 0
```

The producer needs two complete Conway epochs behind the immutable tip.
With the production-like `86400` slot epoch this local proof would take
roughly 48 hours. In `cardano_amaru`, Conway starts at epoch 0 and the
producer can become ready around slot `240`, so local and CI runs can
wait for the real producer completion instead of only proving cluster
startup. The dense active slot coefficient makes enough blocks immutable
inside that short window; without it the immutable ChainDB tip can remain
at genesis even after the slot threshold has passed.

## Topology

```mermaid
flowchart LR
    configurator[configurator] --> p1[p1 cardano-node 10.7.1]
    configurator --> p2[p2 cardano-node 10.7.1]
    configurator --> p3[p3 cardano-node 10.7.1]
    configurator --> relay1[relay1 cardano-node 10.7.1]
    configurator --> relay2[relay2 cardano-node 10.7.1]

    p1 -.N2N.-> p2
    p2 -.N2N.-> p3
    p3 -.N2N.-> p1
    relay1 -.N2N.-> p1
    relay2 -.N2N.-> p1

    p1 --> chain[(p1-state ChainDB)]
    chain --> producer[bootstrap-producer]
    producer --> bundle[(amaru-bundle)]
    bundle --> a1[(a1-state)]
    bundle --> a2[(a2-state)]
    a1 --> amaru1[amaru-relay-1]
    a2 --> amaru2[amaru-relay-2]

    amaru1 -.upstream.-> p1
    amaru2 -.upstream.-> amaru1
```

The Amaru relay nodes do not share writable stores. Each relay entrypoint
waits for the atomically committed bundle, copies it into a private state
volume, and then execs `amaru run`.

## Bootstrap Contract

`bootstrap-producer` runs once:

```text
bootstrap-producer /cardano/state /cardano/config/configs /srv/amaru testnet_42
```

It waits for the producer node's immutable ChainDB to be era-ready, emits
three ledger snapshots for the target window, converts them through
Amaru, extracts headers and nonces, imports all data into Amaru stores,
and atomically commits:

```text
/srv/amaru/testnet_42/
|-- chain.testnet_42.db/
|-- ledger.testnet_42.db/
|-- snapshots/
|-- headers/
`-- nonces.json
```

The ChainDB mount is intentionally read-write:

```yaml
volumes:
  - p1-state:/cardano/state
```

This is not a write contract for the producer. It is required because
cardano-node 10.7.1's consensus ImmutableDB validation path opens chunk
files through APIs that reject read-only filesystems.

## Local Verification

Validate the Compose model:

```bash
INTERNAL_NETWORK=false docker compose -f testnets/cardano_amaru/docker-compose.yaml config
```

Run the standard smoke test:

```bash
./scripts/smoke-test.sh cardano_amaru 600
```

That smoke test proves the cardano-node network, sidecar, and
tx-generator still work with the Amaru services present. For
`cardano_amaru`, it additionally waits for `bootstrap-producer` to exit
`0`, then checks that `amaru-relay-1` and `amaru-relay-2` copied the
bundle into private state volumes and stayed running after `amaru run`
opened those stores.

The same smoke command runs in both the PR image-publish workflow and
the manual smoke workflow, after the existing `cardano_node_master`
smoke.

For a bootstrap-specific cluster run, watch:

```bash
docker compose -f testnets/cardano_amaru/docker-compose.yaml logs -f bootstrap-producer
docker compose -f testnets/cardano_amaru/docker-compose.yaml ps bootstrap-producer amaru-relay-1 amaru-relay-2
```

The success evidence is:

- `bootstrap-producer` prints `wrote /srv/amaru/testnet_42` and exits
  `0`;
- `amaru-relay-1` and `amaru-relay-2` copy the bundle into private state
  volumes;
- `amaru-relay-1` and `amaru-relay-2` enter `amaru run` and remain
  running without a restart during the smoke gate.

## What This Does Not Prove

This stack does not retarget `amaru-bootstrap` to a newer node release.
Moving beyond cardano-node 10.7.1 requires a deliberate upstream
retarget, because ledger CBOR and ChainDB APIs drift laterally across
node releases.

The smoke proof runs on a deliberately short-epoch testnet. It proves the
producer and relay load path for the target node release, but it is not a
production-epoch-duration soak test and does not assign stake to Amaru.
