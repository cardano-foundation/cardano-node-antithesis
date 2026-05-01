# Cardano Amaru Testnet

This testnet extends the `cardano_node_master` shape with an Amaru
bootstrap path:

- three `cardano-node` block producers pinned to the official
  `10.7.1-amd64` image digest;
- two relays, also pinned to the same node release;
- the published `amaru-bootstrap-producer` image from
  `lambdasistemi/amaru-bootstrap`;
- two relay-only Amaru nodes whose entrypoints wait until the producer
  has committed a complete `testnet_42` bootstrap bundle.

Amaru is not assigned stake in this testnet. The only stake-bearing block
producers are the three cardano-node services `p1`, `p2`, and `p3`.
`amaru-relay-1` and `amaru-relay-2` receive no KES key, VRF key, cold
key, operational certificate, or stake-pool genesis assignment.

This testnet uses a fast bootstrap profile:

```yaml
protocolConsts:
  k: 10
epochLength: 120
securityParam: 10
activeSlotsCoeff: 0.2
TestConwayHardForkAtEpoch: 0
```

The producer requires two complete Conway epochs behind the immutable
tip. These values make that proof bounded for local and CI runs without
changing the cardano-node release target or giving Amaru producer
credentials. Dense block production is part of the profile because the
producer reads immutable chunks only; sparse block production can leave
the immutable tip at genesis for too long.

The Amaru relay containers are intentionally quiet for Antithesis log
ingestion: compose sets `AMARU_LOG=warn`, `AMARU_TRACE=warn`, and
`AMARU_COLOR=never`, and the wrapper loop does not print bundle-wait
heartbeats.

The producer image is pinned by full source commit SHA:

```text
ghcr.io/lambdasistemi/amaru-bootstrap-producer:d81dd7d31e1c23b3223d3c4155294b82dc56ea0e
```

The cardano-node release image is pinned in Compose by digest only:

```text
ghcr.io/intersectmbo/cardano-node@sha256:3275d357053d21f3220f74b0854fd584e1fe322dfa1bbb78effd760c3191d14c
```

That digest is the manifest digest for the upstream
`ghcr.io/intersectmbo/cardano-node:10.7.1-amd64` tag. Compose must not
use the Docker-valid `repo:tag@sha256:digest` spelling here because the
Antithesis image parameter validator accepts image names by tag or by
digest, but rejects the combined tag-plus-digest form before the cluster
is built.

The tx-generator image is pinned to the component rebuild from this
repository commit:

```text
ghcr.io/cardano-foundation/cardano-node-antithesis/tx-generator:ba7697e
```

That rebuild consumes
`lambdasistemi/cardano-node-clients@898a2c470ced6a82fa5a32b18cbaf195e1cce927`,
the main merge commit for
`https://github.com/lambdasistemi/cardano-node-clients/pull/105`. The
important runtime behavior is the N2C reconnect supervisor and typed
handling of GHC's `BlockedIndefinitelyOnSTM` detection, so a relay
disconnect during LSQ or local transaction submission returns a
recoverable control response instead of killing the daemon. The
Antithesis composer also records transient control-socket gaps as
reachable telemetry rather than as an SDK failure, because fault
scheduling can run a command before the daemon is ready or while the
upstream relay is being disrupted.

## Runtime Flow

```text
configurator -> p1/p2/p3/relay1/relay2
                  |
                  v
             p1 ChainDB
                  |
                  v
       bootstrap-state-snapshot
                  |
                  v
          bootstrap ChainDB copy
                  |
                  v
          bootstrap-producer
                  |
                  v
           amaru-bundle volume
             |             |
             v             v
      amaru-relay-1   amaru-relay-2
```

`bootstrap-state-snapshot` waits for `p1` to reach slot `360` through
the local node socket, then copies `p1`'s ChainDB into an isolated
`bootstrap-state` volume. `bootstrap-producer` mounts that copy
read-write at `/cardano/state`. This preserves the cardano-node 10.7.1
consensus API requirement that immutable chunk validation has write
permissions, while avoiding a second process opening the live `p1`
ChainDB during Antithesis fault scheduling.

Each relay-only Amaru entrypoint copies the final bundle into its private
state volume before it execs `amaru run`, so the two Amaru nodes do not
share writable chain or ledger stores.

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
tx-generator still start and converge. For `cardano_amaru`, it also
waits for `bootstrap-producer` to complete and checks that both
relay-only Amaru nodes copied the bundle into private state and stayed
running after `amaru run` opened the stores.

For the bootstrap-specific proof, inspect:

```bash
docker compose -f testnets/cardano_amaru/docker-compose.yaml logs -f bootstrap-producer
docker compose -f testnets/cardano_amaru/docker-compose.yaml ps bootstrap-producer amaru-relay-1 amaru-relay-2
```

Expected completion sequence:

1. `bootstrap-producer` prints `wrote /srv/amaru/testnet_42` and exits
   `0`.
2. `amaru-relay-1` and `amaru-relay-2` copy the bundle into their private state
   volumes.
3. `amaru-relay-1` and `amaru-relay-2` start with `amaru run` and stay
   running without a restart during the smoke gate.

## Compatibility Constraint

This stack intentionally targets cardano-node 10.7.1. The Amaru
bootstrap CBOR projection is release-sensitive, so a green compile
against a different ledger dependency set is not evidence that the
runtime bytes are compatible with this cluster.
