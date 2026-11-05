As a testnet operator, I want the Asteria binaries and indexer to use a compatible node client so that cardano-node-head runs exercise gameplay.

The November 5 HEAD run fbc8893a40ffc5b130f6b4f787448e52-64-11 at 67b7d1da recorded zero successful heartbeats and 37,339 false observations. The indexer responds with null processedSlot/tipSlot and remains in readiness retries. Release control run 995de3f3208df1ffe65f29b35b320b8b-64-11 has 41,634 successful heartbeats.

Asteria image bf48615 pins cardano-node-clients 428313d, which offers N2C v20. Node HEAD 2e76d83f uses consensus 5.2 offering only v23. Client main partially fixes this, with standalone ChainSync still requiring lambdasistemi/cardano-node-clients#223.

Update both the Cabal source pin (bootstrap/player/invariant) and flake input (indexer), align transitive dependency pins, build a new image and repin consumers. Add an executable smoke covering indexer readiness and slot progress against the actual HEAD and release node images. Keep heartbeat expectations intact. Hosted acceptance requires a fresh Antithesis run and unchanged or improved findings.

Operator ruling: use the complete node 11.2.0 ecosystem and N2C v23, following cardano-balance-transaction PR51. The released mixed network uses its existing compatible relay2; no v20-only socket support is claimed. Keep older node images and all game/composer assertions. Clients e17ffdc74c756680006d43963e337d9a766aa144 (PR224) and tools2e0d7cb813a9509c6677debdfe5fdad2a76351ff (PR170) are immutable published dependency candidates. Ledger-read's exact pinned revision is now merged upstream.
