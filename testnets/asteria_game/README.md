# asteria_game testnet

Same producer/relay topology as `cardano_node_master`, but with the
`asteria-game` workload (PR #67's lifted bootstrap + player) instead
of the green-baseline `asteria-stub`. Kept as a separate testnet so
asteria iteration can land on `main` without disturbing the canonical
scheduled run.

See [docs](https://cardano-foundation.github.io/cardano-node-antithesis/testnets/cardano-node-master/).
