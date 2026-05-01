# Adversary roadmap

Long-term plan for adversarial workload against the testnet. The
[adversary component][adversary-doc] is now a long-running daemon
(see Tier 1.1 below); this page tracks where it goes next.

## Status

| Tier | Archetype | Status |
|---|---|---|
| 1.1 | `chain_sync_flap` | **landed** ([clients PR #103][pr-103] scaffold, [#106][pr-106] real impl, [antithesis PR #99][pr-99] consumer) |
| 1.2 | `chain_sync_thrash` | next — see [`specs/036-cardano-adversary-thrash/`][thrash-spec] in cardano-node-clients |
| 1.3 | `chain_sync_slow_loris` | not started |
| 2 | `block_fetch_replay`, `tx_submission_*`, `keepalive_abuse` | not started |
| 3 | upstream-peer Byzantine adversary | epic [#91][issue-91], not started |
| 4 | `lsq_flood`, `local_tx_submission_garbage` | not started |

## Goals

1. **Move the adversary from one-shot to long-running.** ✅ Done in
   [PR #99][pr-99]. The daemon now listens on a UNIX control socket,
   composer drivers fire one NDJSON request per Antithesis tick.
2. **Cover more of the protocol surface than chain-sync.** In
   progress: `chain_sync_flap` lives; thrash + slow-loris next; then
   block-fetch, tx-submission, keep-alive, handshake, and upstream
   (responder).
3. **Steer adversarial choices with the Antithesis hypervisor.** ✅
   In place since PR #103. Every randomness draw routes through the
   `RandomSource` typeclass; default impl shells out to
   `antithesis_random` when present and falls back to `System.Random`
   otherwise.
4. **Move the daemon home to [`lambdasistemi/cardano-node-clients`][cnc].**
   ✅ Done. The package lives next to `cardano-tx-generator` and
   shares its `Provider` / `Submitter` / `TxBuild` library surface
   plus the same release flow.

## Architecture

```
                   +---------------------------------+
                   |    cardano-adversary daemon     |
                   |    (Haskell, in cardano-node-   |
                   |    clients)                     |
                   |                                 |
   composer        |   - per-request N2N initiator   |
   parallel_       |     connections to producers    |
   driver_*.sh --->|   - reads chain points from     |
   over NDJSON     |     tracer-sidecar's file       |
   on UNIX socket  |   - exposes /state/adversary-   |
                   |     control.sock                |
                   |   - one request → one           |
                   |     adversarial action          |
                   +---------------------------------+
```

### Mapping from tx-generator (now established)

| tx-generator | cardano-adversary |
|---|---|
| `cardano-tx-generator` daemon | `cardano-adversary` daemon |
| `/state/tx-generator-control.sock` | `/state/adversary-control.sock` |
| `parallel_driver_transact.sh` / `_refill.sh` | `parallel_driver_flaky_chain_sync.sh` (Tier 1.1) |
| `eventually_population_grew.sh` | (Tier 2 will add an `eventually_adversary_active.sh`) |
| `finally_pressure_summary.sh` | (Tier 2 will add a `finally_adversary_summary.sh`) |
| Wire spec at [`specs/034-cardano-tx-generator/contracts/control-wire.md`][txg-wire] | Wire spec at [`specs/036-cardano-adversary/contracts/control-wire.md`][adv-wire] |

### Implementation seams that exist today

- `Cardano.Node.Client.Adversary.{Application,ChainSync.Codec,ChainSync.Connection,ChainPoints}`
  — the lifted N2N initiator library that backs `chain_sync_flap`
  and will back the rest of Tier 1.
- `Cardano.Node.Client.Adversary.RandomSource` — typeclass +
  Antithesis-aware default (`antithesis_random` CLI when present,
  `System.Random` fallback). Per-request determinism via
  `splitFromSeed :: Word64 -> StdGen`.
- `Cardano.Node.Client.Adversary.Server` — NDJSON dispatcher. New
  endpoints register themselves by extending `ServerHooks`.
- `Cardano.Node.Client.Provider` / `Submitter` / `TxBuild` —
  available for the N2C-side adversaries (mempool flooding,
  malformed tx submission) when Tier 2/4 lands.
- A responder-library half for upstream-peer mode is **not** in the
  tree yet; it arrives with Tier 3.

## Tier list of misbehaviour archetypes

Each archetype = one daemon endpoint + one composer
`parallel_driver_*.sh` + a documented invariant the cluster must
preserve under it. Order is implementation order, easiest first.

### Tier 1 — port the existing surface into the daemon

1. **`chain_sync_flap`** ✅ — pick random intersection point, sync
   `LIMIT` blocks, disconnect. Replaces the stand-alone `adversary`
   binary that lived in `cardano-foundation/cardano-node-antithesis`.
2. **`chain_sync_thrash`** — same connection, repeatedly
   `MsgFindIntersect` to a different random point without
   completing the sync. Stresses the producer's intersection-finding
   cache. Spec lives in
   [`lambdasistemi/cardano-node-clients/specs/036-cardano-adversary-thrash/`][thrash-spec].
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

Tracked as its own epic ([#91][issue-91]). Cluster nodes must dial
the adversary, so the adversary becomes a node-to-node *server*.
Then:

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

| # | Repo | Description | Status |
|---|---|---|---|
| A | antithesis | Refresh `docs/components/adversary.md`, publish this roadmap | ✅ [#88][pr-88] |
| B | clients | Scaffold `cardano-adversary` daemon | ✅ [#103][pr-103] |
| C | clients | `chain_sync_flap` endpoint (port from antithesis-side) | ✅ [#106][pr-106] |
| D | antithesis | Switch `components/adversary/` to consume the new image; delete the standalone binary | ✅ [#99][pr-99] |
| E | clients | `chain_sync_thrash` endpoint (Tier 1.2) | spec written, impl next |
| F | clients | `chain_sync_slow_loris` endpoint (Tier 1.3) | not started |

After Tier 1 is fully green on Antithesis: start Tier 2 issues, one
endpoint per PR. Tier 3 waits on appetite for the topology variant.

## Tickets

Filed in [`cardano-foundation/cardano-node-antithesis`][repo]:

- ✅ [#87][issue-87] — refresh adversary docs (closed by PR #88).
- [#89][issue-89] — adversary roadmap epic (still open; tracks the rest of Tier 1+).
- ✅ [#90][issue-90] — switch to consuming the upstream daemon image (closed by PR #99).
- [#91][issue-91] — Tier 3 upstream-peer epic.

Filed in [`lambdasistemi/cardano-node-clients`][cnc]:

- ✅ [#102][cli-102] — daemon scaffold (closed by PR #103).
- ✅ [#104][cli-104] — `chain_sync_flap` (closed by PR #106).
- [#107][cli-107] — `chain_sync_thrash` (Tier 1.2, next up).

<!-- MARKDOWN LINKS & IMAGES -->

[adversary-doc]: adversary.md
[tx-generator-doc]: https://github.com/cardano-foundation/cardano-node-antithesis/tree/main/components/tx-generator
[cnc]: https://github.com/lambdasistemi/cardano-node-clients
[repo]: https://github.com/cardano-foundation/cardano-node-antithesis
[adv-wire]: https://github.com/lambdasistemi/cardano-node-clients/blob/main/specs/036-cardano-adversary/contracts/control-wire.md
[txg-wire]: https://github.com/lambdasistemi/cardano-node-clients/blob/main/specs/034-cardano-tx-generator/contracts/control-wire.md
[thrash-spec]: https://github.com/lambdasistemi/cardano-node-clients/tree/main/specs/036-cardano-adversary-thrash
[pr-88]: https://github.com/cardano-foundation/cardano-node-antithesis/pull/88
[pr-99]: https://github.com/cardano-foundation/cardano-node-antithesis/pull/99
[pr-103]: https://github.com/lambdasistemi/cardano-node-clients/pull/103
[pr-106]: https://github.com/lambdasistemi/cardano-node-clients/pull/106
[issue-87]: https://github.com/cardano-foundation/cardano-node-antithesis/issues/87
[issue-89]: https://github.com/cardano-foundation/cardano-node-antithesis/issues/89
[issue-90]: https://github.com/cardano-foundation/cardano-node-antithesis/issues/90
[issue-91]: https://github.com/cardano-foundation/cardano-node-antithesis/issues/91
[cli-102]: https://github.com/lambdasistemi/cardano-node-clients/issues/102
[cli-104]: https://github.com/lambdasistemi/cardano-node-clients/issues/104
[cli-107]: https://github.com/lambdasistemi/cardano-node-clients/issues/107
