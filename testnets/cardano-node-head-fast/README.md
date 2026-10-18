# Description

The [cardano-node-releases](../cardano-node-releases/README.md) cluster with a
fast genesis, so a run crosses epoch boundaries.

| parameter | releases | releases-fast |
|---|---|---|
| `securityParam` (k) | 432 | 20 |
| `activeSlotsCoeff` (f) | 0.05 | 0.2 |
| `slotLength` | 1 s | 0.5 s |
| `epochLength` | 86400 slots (24 h) | 1000 slots (500 s) |

`epochLength` stays at 10k/f, the Praos sizing that keeps the stability window
(3k/f) and the randomness stabilisation window (4k/f) inside the epoch. The
cost of the short epoch is a short fork tolerance: k blocks are about 50 s of
producer divergence.

The tracer-sidecar receives `SECURITY_PARAM: "20"`. The asteria-game indexer
keeps its baked-in k = 432, which only lengthens its rollback log.

see [docs](https://cardano-foundation.github.io/cardano-node-antithesis/testnets/cardano-node-releases/)
