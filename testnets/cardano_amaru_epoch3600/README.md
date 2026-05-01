# Cardano Amaru 3600-Second Epoch Testnet

This testnet is the Antithesis-only long-epoch variant of
`cardano_amaru`. It keeps the same Amaru bootstrap path:

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

This testnet uses a one-hour epoch bootstrap profile:

```yaml
protocolConsts:
  k: 10
epochLength: 3600
securityParam: 10
activeSlotsCoeff: 0.2
TestConwayHardForkAtEpoch: 0
```

The producer requires two complete Conway epochs behind the immutable
tip. With `epochLength: 3600`, the first useful snapshot is expected only
after roughly two hours of chain slot time. This variant is therefore
intended for Antithesis runs, where a one-hour campaign can cover about
48 hours of test time, not for GitHub's wall-clock smoke-test job. Dense
block production is still part of the profile because the producer reads
immutable chunks only; sparse block production can leave the immutable
tip at genesis for too long.

The Amaru relay containers are intentionally quiet for Antithesis log
ingestion: compose sets `AMARU_LOG=warn`, `AMARU_TRACE=warn`, and
`AMARU_COLOR=never`, and the wrapper loops do not print polling
heartbeats. The bootstrap producer stores per-attempt logs under
`/srv/amaru/.logs` in the bundle volume and prints only the final commit
line or a bounded tail for a non-retryable failure.

The producer image is pinned by full source commit SHA:

```text
ghcr.io/lambdasistemi/amaru-bootstrap-producer:pr-32-ad64e76778b0408ec66f353c7e58c8a1e7d4045f
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

The generic observability and assertion services remain enabled:
`tracer`, `tracer-sidecar`, `log-tailer`, and `sidecar`. The
transaction perturbator workload is deliberately absent; this testnet has
no `tx-generator` service.

## Runtime Flow

```text
configurator -> p1/p2/p3/relay1/relay2
                  |
                  v
             p1 ChainDB
                  |
                  v
      bootstrap-producer refresh loop
                  |
                  v
          bootstrap ChainDB copy
                  |
                  v
           amaru-bundle volume
             |             |
             v             v
      amaru-relay-1   amaru-relay-2
```

`bootstrap-producer` owns the snapshot-refresh loop. It mounts `p1`'s
live ChainDB read-only at `/live`, copies it into the isolated
`bootstrap-state` volume, then mounts that copy read-write at
`/cardano/state` for the upstream producer command. This preserves the
cardano-node 10.7.1 consensus API requirement that immutable chunk
validation has write permissions, while avoiding writes to the live `p1`
ChainDB during Antithesis fault scheduling. Retryable readiness and copy
failures use a short per-attempt deadline and refresh the snapshot inside
the same container instead of exiting non-zero.

Each relay-only Amaru entrypoint copies the final bundle into its private
state volume before it execs `amaru run`, so the two Amaru nodes do not
share writable chain or ledger stores.

Each relay also writes a startup marker into the shared `amaru-startup`
volume immediately before the `amaru run` exec. The sidecar mounts that
volume and the `composer/amaru` scripts into the existing
`convergence` Test Composer template so Antithesis can score
`parallel_driver_amaru_started.sh` and `finally_amaru_started.sh` as
explicit properties instead of asking readers to infer startup from
container background-monitor logs.

## Local Commands

Validate the Compose model:

```bash
INTERNAL_NETWORK=false docker compose -f testnets/cardano_amaru_epoch3600/docker-compose.yaml config
```

Do not add this testnet to the default GitHub smoke-test matrix. A local
wall-clock smoke run needs a much larger bootstrap timeout than the fast
`cardano_amaru` profile:

```bash
AMARU_BOOTSTRAP_SMOKE_TIMEOUT=9000 ./scripts/smoke-test.sh cardano_amaru_epoch3600 600
```

The smoke test proves the cardano-node network and sidecar convergence
checks still start. Because this profile has no `tx-generator`, the
generic tx-generator smoke gate is skipped. For
`cardano_amaru_epoch3600`, the smoke also waits for
`bootstrap-producer` to complete, checks that both relay-only Amaru nodes
copied the bundle into private state and stayed running after
`amaru run` opened the stores, then executes the same Amaru startup
property that Antithesis discovers under
`/opt/antithesis/test/v1/convergence/`.

For the bootstrap-specific proof, inspect:

```bash
docker compose -f testnets/cardano_amaru_epoch3600/docker-compose.yaml logs -f bootstrap-producer
docker compose -f testnets/cardano_amaru_epoch3600/docker-compose.yaml ps bootstrap-producer amaru-relay-1 amaru-relay-2
```

Expected completion sequence:

1. `bootstrap-producer` prints `wrote /srv/amaru/testnet_42` and exits
   `0`.
2. `amaru-relay-1` and `amaru-relay-2` copy the bundle into their private state
   volumes.
3. `amaru-relay-1` and `amaru-relay-2` start with `amaru run` and stay
   running without a restart during the smoke gate.
4. `parallel_driver_amaru_started.sh` emits `amaru_relays_started` when
   it samples both relay startup markers; `finally_amaru_started.sh`
   fails the run if those markers are still missing at the final check.
   Both commands run from the `convergence` template so Antithesis does
   not mix separate test directories in one sidecar.
5. The sidecar uses a larger post-fault convergence budget in this
   profile (`30s` settle, `15` attempts, `3s` delay) so a final check is
   not scored while producer tips are still catching up after faults
   stop.

## Compatibility Constraint

This stack intentionally targets cardano-node 10.7.1. The Amaru
bootstrap CBOR projection is release-sensitive, so a green compile
against a different ledger dependency set is not evidence that the
runtime bytes are compatible with this cluster.
