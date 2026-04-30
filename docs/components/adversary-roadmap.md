# Adversary roadmap

Long-term plan for adversarial workload against the testnet. The
[adversary component][adversary-doc] today is a one-shot chain-sync
client; this page is the north star for where it is going.

## Goals

1. **Move the adversary from one-shot to long-running.** Mirror the
   [tx-generator][tx-generator-doc] design: a daemon listening on a
   UNIX control socket, with composer scripts firing one NDJSON
   request per Antithesis tick.
2. **Cover more of the protocol surface than chain-sync.** Add
   block-fetch, tx-submission, keep-alive, handshake, and N2C
   adversaries — and an upstream-peer (server) mode for the canonical
   "Byzantine peer" tests.
3. **Steer adversarial choices with the Antithesis hypervisor.** Every
   randomness draw must come from an injectable `RandomSource` so
   Antithesis can bias the run.
4. **Move the daemon home to [`lambdasistemi/cardano-node-clients`][cnc].**
   The tx-generator daemon already lives there; adversary inherits
   the same `Provider`, `Submitter`, `TxBuild`, and control-wire
   conventions, and the same release flow. This repo keeps only the
   image-tag pin, the composer drivers, and the compose wiring.

## Architecture target

```
                   +---------------------------------+
                   |    cardano-adversary daemon     |
                   |    (Haskell, in cardano-node-   |
                   |    clients)                     |
                   |                                 |
   composer        |   - keeps a pool of N2N         |
   parallel_       |     initiator connections to    |
   driver_*.sh --->|     producers                   |
   over NDJSON     |   - keeps a pool of N2C         |
   on UNIX socket  |     connections to relays       |
                   |   - exposes /state/adversary-   |
                   |     control.sock                |
                   |   - one request → one           |
                   |     adversarial action          |
                   |   - emits SDK reachable/        |
                   |     sometimes/always per action |
                   +---------------------------------+
```

### Mapping from tx-generator

| tx-generator (today) | cardano-adversary (planned) |
|---|---|
| `cardano-tx-generator` daemon | `cardano-adversary` daemon |
| `/state/tx-generator-control.sock` | `/state/adversary-control.sock` |
| `parallel_driver_transact.sh` / `_refill.sh` | one driver script per misbehaviour archetype |
| `eventually_population_grew.sh` | `eventually_adversary_active.sh` |
| `finally_pressure_summary.sh` | `finally_adversary_summary.sh` |
| Wire spec at `specs/034-cardano-tx-generator/contracts/control-wire.md` | Wire spec at `specs/0XX-cardano-adversary/contracts/control-wire.md` |

### Implementation seams to reuse from `cardano-node-clients`

- `Cardano.Node.Client.Provider` / `Submitter` / `TxBuild` — for the
  N2C-side adversaries (mempool flooding, malformed tx submission).
- `Adversary.ChainSync.Connection` from this repo becomes the
  initiator-library half of an N2N adversary library; we add a
  responder-library half for upstream-peer mode.
- A single `RandomSource` typeclass — the one already used by the
  asteria-player at
  [`components/asteria-player/src/Asteria/RandomSource.hs`][rs] —
  with an `antithesis_random` CLI implementation and a
  `System.Random` fallback. Every adversarial decision draws from
  it.

## Tier list of misbehaviour archetypes

Each archetype = one daemon endpoint + one composer
`parallel_driver_*.sh` + a documented invariant the cluster must
preserve under it. Order is implementation order, easiest first.

### Tier 1 — port the existing surface into the daemon

1. **`chain_sync_flap`** — what we have today: pick random
   intersection point, sync `LIMIT` blocks, disconnect. Becomes a
   single endpoint of the new daemon. Removes the stand-alone
   `adversary` binary.
2. **`chain_sync_thrash`** — same connection, repeatedly
   `MsgFindIntersect` to a different random point without finishing
   the sync. Stresses the producer's intersection-finding cache.
3. **`chain_sync_slow_loris`** — open the connection, complete
   handshake, then send `MsgRequestNext` at a deliberately slow
   cadence; assert the connection is kept alive (or assert it gets
   evicted, whichever the protocol promises).

### Tier 2 — other mini-protocols, downstream side

4. **`block_fetch_replay`** — `BlockFetch` client that requests
   already-fetched ranges back-to-back, plus ranges straddling
   rollback boundaries.
5. **`tx_submission_flood`** — N2N `TxSubmission2` client that
   announces tx-ids the producer has not requested, or refuses to
   deliver bodies after announcing them.
6. **`tx_submission_garbage`** — submits well-formed-CBOR but
   ledger-invalid txs at high rate (mempool pressure).
7. **`keepalive_abuse`** — `KeepAlive` cookies out of order or never
   replied to.

### Tier 3 — upstream-peer mode (the canonical Byzantine peer)

This is the big one — promised by the original component pitch, never
implemented. Cluster nodes must dial the adversary, so the adversary
becomes a node-to-node *server*. Then:

8. **`upstream_fork_serve`** — announce a tip on the honest chain's
   history but `MsgRollForward` with a fabricated header. ChainSel
   must discard.
9. **`upstream_equivocate`** — serve two contradictory headers at
   the same slot to two peers.
10. **`upstream_too_far_ahead`** — announce a tip 10× past the real
    tip; producer must not fetch.
11. **`upstream_long_rollback`** — induce a rollback past *k* and
    verify the honest peer rejects.

Tier 3 needs a topology change (relays peer with adversary in their
`topology.json`). That is a separate testnet variant; it does not
disturb the current `cardano_node_master` testnet.

### Tier 4 — N2C abuse (lower priority)

12. **`lsq_flood`** — open many `LocalStateQuery` sessions against a
    relay, hold them.
13. **`local_tx_submission_garbage`** — N2C variant of #6.

## Sequenced PR plan

- **PR A (this repo, doc-only)** — refresh
  `docs/components/adversary.md` to match implementation; publish
  this roadmap. *No behaviour change.*
- **PR B (cardano-node-clients)** — scaffold `cardano-adversary`
  package next to `cardano-tx-generator`, with `Provider`/`Submitter`
  reuse, control-wire spec, daemon skeleton emitting
  `sdk_reachable` only.
- **PR C (cardano-node-clients)** — implement `chain_sync_flap`
  endpoint by porting `Adversary.Application` into the daemon. Add
  `RandomSource` plumbing.
- **PR D (this repo)** — switch `components/adversary/` to consume
  the new image, update compose + composer drivers to the daemon
  shape; delete the stand-alone Haskell binary.
- **PR E (cardano-node-clients)** — `chain_sync_thrash` +
  `chain_sync_slow_loris` endpoints. Tier 1 complete.
- After Tier 1 is green on Antithesis: start Tier 2 issues, one
  endpoint per PR.
- Tier 3 starts when there is appetite for the topology variant;
  track as its own parent epic with sub-issues per archetype.

## Tickets

Filed in
[`cardano-foundation/cardano-node-antithesis`][repo]:

- "Adversary roadmap: long-running daemon + parallel-driver fan-out
  (epic)" — links to this page.
- "Move adversary daemon home to cardano-node-clients; keep only
  image + composer drivers here" (PR D driver).
- "Tier 3: upstream-peer Byzantine adversary testnet variant"
  (separate epic).

To be filed in
[`lambdasistemi/cardano-node-clients`][cnc]:

- "Scaffold cardano-adversary daemon (PR B)".
- "Port chain_sync_flap into adversary daemon (Tier 1.1)".
- "chain_sync_thrash endpoint (Tier 1.2)".
- "chain_sync_slow_loris endpoint (Tier 1.3)".
- "block_fetch_replay endpoint (Tier 2.4)".
- "tx_submission_{flood,garbage} endpoints (Tier 2.5–2.6)".
- "keepalive_abuse endpoint (Tier 2.7)".
- "Upstream-peer responder library + topology variant (Tier 3 epic)".

<!-- MARKDOWN LINKS & IMAGES -->

[adversary-doc]: adversary.md
[tx-generator-doc]: https://github.com/cardano-foundation/cardano-node-antithesis/tree/main/components/tx-generator
[cnc]: https://github.com/lambdasistemi/cardano-node-clients
[rs]: https://github.com/cardano-foundation/cardano-node-antithesis/blob/main/components/asteria-player/src/Asteria/RandomSource.hs
[repo]: https://github.com/cardano-foundation/cardano-node-antithesis
