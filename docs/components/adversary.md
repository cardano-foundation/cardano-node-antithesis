# Adversary

A node-to-node downstream client that connects to producer nodes, asks
for a chain sync from a randomly-chosen intersection point, pulls a
bounded number of blocks, and disconnects. The composer fires it
repeatedly under fault injection so producers see concurrent connection
churn while their chain is being rolled back, paused, or partitioned.

## What it does today

The component ships a single Haskell binary, `adversary`, run inside
the `sidecar` container.

- Initiator-only N2N client (`NodeToNodeV_14`,
  `InitiatorOnlyDiffusionMode`).
- Implements the **chain-sync mini-protocol only** (mini-protocol
  number 2).
- Reads a list of chain points from `CHAINPOINT_FILEPATH`. The file is
  harvested by `tracer-sidecar` from real cardano-tracer logs at run
  time; one chain point per line, plus the implicit `origin` point.
- Spawns `NCONNS` concurrent connections fanned out over the producer
  hostnames in `NODES`. Each connection picks a random starting point,
  sends `MsgFindIntersect [point]`, then loops `MsgRequestNext` until
  `LIMIT` blocks have been pulled or the producer's tip is reached,
  then disconnects.
- All connections terminate. The driver script exits 0 on completion.
- Randomness comes from `newStdGen` (host RNG entropy). It is **not**
  wired to the Antithesis hypervisor's random source today.

This exercises:

- Producer mux / connection-management code under N concurrent N2N
  initiators.
- The intersection-finding logic, with both well-known and historic /
  rolled-back points.
- ChainSync server state-machine handling under concurrent peers.

This does **not** exercise:

- Block-fetch, tx-submission, keep-alive, or handshake mini-protocols.
- Upstream-peer (server) behaviour — the adversary never serves a chain.
- Mid-protocol misbehaviour — the loop only ever sends well-formed
  requests.
- Steered fault injection — Antithesis cannot bias the adversary's
  choices.

The full long-term plan is in [adversary-roadmap.md](adversary-roadmap.md).

## Composer driver

`parallel_driver_flaky_chain_sync.sh` lives at
`components/sidecar/composer/chain-sync-client/` and is mounted into
the `sidecar` container at
`/opt/antithesis/test/v1/chain-sync-client/`. It:

- Resolves the producer hostnames from `POOLS` (and optional
  `EXTRA_NODES`).
- Validates that `CHAINPOINT_FILEPATH` exists. If the file is missing
  the script exits 0, so an early-test invocation while
  `tracer-sidecar` is still warming up does not fail the run (see
  [#9][issue-9]).
- Execs the `adversary` binary with the resolved arguments.

A duplicate of the same script lives at
`components/adversary/composer/chain-sync-client/` for the component's
local test loop; the canonical copy is the one in
`components/sidecar/`.

## Environment variables

| Variable | Default | Meaning |
|----------|---------|---------|
| `POOLS` | (required) | Number of producer pools; the script generates `p1`, `p2`, ..., `p$POOLS` as target hostnames |
| `EXTRA_NODES` | empty | Whitespace-separated extra hostnames to append (e.g. relays) |
| `PORT` | `3001` | Producer N2N port |
| `NETWORKMAGIC` | `42` | Cardano network magic for handshake |
| `LIMIT` | `100` | Maximum number of blocks pulled per connection before disconnecting |
| `NCONNS` | `100` | Number of concurrent connections per invocation |
| `CHAINPOINT_FILEPATH` | (required) | File written by `tracer-sidecar` listing harvested chain points |

## Wiring on `cardano_node_master`

The adversary runs inside the `sidecar` service. The relevant compose
fragment in [`testnets/cardano_node_master/docker-compose.yaml`][compose]:

```yaml
sidecar:
  image: ghcr.io/cardano-foundation/cardano-node-antithesis/sidecar@sha256:...
  environment:
    NETWORKMAGIC: 42
    PORT: 3001
    LIMIT: 100
    POOLS: "3"
    NCONNS: 1
    CHAINPOINT_FILEPATH: "/opt/cardano-tracer/chainPoints.log"
  volumes:
    - tracer:/opt/cardano-tracer
```

Note `NCONNS: 1` on the deployed testnet — the value is intentionally
conservative until we have signal on whether higher fan-out causes
spurious findings.

## Build the image

The adversary is published as part of the `sidecar` image (see
[`components/sidecar/`][sidecar-comp]). It is also buildable
standalone for local development.

### Nix

```bash
cd components/adversary
nix build .#docker-image
docker load < ./result
```

### Non-Nix

```bash
cd components/adversary
docker build \
    -t ghcr.io/cardano-foundation/cardano-node-antithesis/adversary:dev \
    -f ./Dockerfile .
```

## Local test loop

```bash
just up
docker compose -f testnets/cardano_node_master/docker-compose.yaml \
    exec sidecar bash
/opt/antithesis/test/v1/chain-sync-client/parallel_driver_flaky_chain_sync.sh
```

## Source layout

```
components/adversary/
├── adversary.cabal
├── app/Main.hs                          — argv parsing, point file load
├── src/Adversary.hs                     — reexports + chain-point parse
├── src/Adversary/Application.hs         — chain-sync state machine,
│                                          repeatedAdversaryApplication
├── src/Adversary/ChainSync/
│   ├── Codec.hs                         — codec for Header/Point/Tip
│   └── Connection.hs                    — connectToNode + Ouroboros
├── composer/chain-sync-client/
│   └── parallel_driver_flaky_chain_sync.sh
├── test/                                — AdversarySpec.hs unit tests
├── flake.nix
└── Dockerfile
```

## See also

- [Adversary roadmap](adversary-roadmap.md) — long-term plan: long-running daemon, parallel-driver fan-out, full tier list of misbehaviour archetypes, planned home in `lambdasistemi/cardano-node-clients`.
- [#9][issue-9] — the chainpoint-file race that the driver tolerates.
- [Network spec PDF][Netspec] — chain-sync mini-protocol on p. 21.
- [ouroboros-network][Ouroboros] — the upstream library we depend on.
- Other projects by [HAL][HAL]
- Other projects by the [Cardano Foundation][CF]
- About [Cardano][Cardano]

<!-- MARKDOWN LINKS & IMAGES -->

[Ouroboros]: https://github.com/IntersectMBO/ouroboros-network
[Netspec]: https://ouroboros-network.cardano.intersectmbo.org/pdfs/network-spec/network-spec.pdf
[issue-9]: https://github.com/cardano-foundation/cardano-node-antithesis/issues/9
[sidecar-comp]: https://github.com/cardano-foundation/cardano-node-antithesis/tree/main/components/sidecar
[compose]: https://github.com/cardano-foundation/cardano-node-antithesis/blob/main/testnets/cardano_node_master/docker-compose.yaml
[HAL]: https://github.com/cardano-foundation/hal
[CF]: https://github.com/cardano-foundation
[Cardano]: https://cardano.org/
