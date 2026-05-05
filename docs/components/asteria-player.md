# Asteria Player

Long-running container that plays the [asteria][asteria] game inside the
cardano-node-antithesis cluster. Drives realistic transaction traffic
(spend script, mint, reference inputs, validity bounds, inline datums)
through the cluster nodes so antithesis can fuzz state-space exploration
against contention between players.

This component is part of the phase-1 gatherer feature ([#56][issue-56]).

## Adding asteria-game to a testnet

Drop the asteria-game container into any testnet's
`docker-compose.yaml` to give Antithesis a real workload generator on
top of an idle Cardano cluster. The container bundles a long-lived
utxo-indexer plus three short-lived binaries fired by composer
scripts (bootstrap, player, invariant) — see
[the why-it's-here section in the master testnet doc][master-why]
for the rationale.

### Pre-requisites

The host testnet must already have:

- A relay that block producers connect to, e.g. `relay1`, with a
  named volume mounted at `/state` so the asteria-game container
  can read its node socket. The master testnet uses
  `relay1-state:/state`.
- A `utxo-keys` named volume populated by the configurator with
  the genesis wallet skeys (the bootstrap binary needs them to
  sign the deploy tx).
- A network magic of `42`. Other magics are not currently wired —
  `NETWORK_MAGIC` is hard-coded in the player's compose env.

### Compose block

Add this service block under `services:` at the bottom of your
`docker-compose.yaml`:

```yaml
  asteria-game:
    image: ghcr.io/cardano-foundation/cardano-node-antithesis/asteria-game:<tag>
    container_name: asteria-game
    hostname: asteria-game.example
    environment:
      INDEXER_SOCK: /tmp/idx.sock
      CARDANO_NODE_SOCKET_PATH: /state/node.socket
      NETWORK_MAGIC: "42"
    volumes:
      - relay1-state:/state:ro            # read-only N2C socket
      - utxo-keys:/utxo-keys:ro           # genesis skeys for bootstrap
      - asteria-game-db:/idx-db           # RocksDB persistence
      - asteria-deploy:/asteria-deploy    # per-deploy seed TxIn
    tmpfs:
      - /tmp                              # holds /tmp/idx.sock
    depends_on:
      relay1:
        condition: service_started
```

Then declare the two new named volumes under the top-level
`volumes:` block:

```yaml
volumes:
  # … existing volumes …
  asteria-game-db:
  asteria-deploy:
```

`<tag>` is a 7-char short SHA from a commit on this repo. The
`publish-images` workflow rebuilds the image and pushes
`asteria-game:<short-sha>` whenever a commit touches
`components/asteria-game/`. Pick the tag from any green
`publish-images` run on `main`, or use the digest pinned by master
in [`testnets/cardano_node_master/docker-compose.yaml`][master-compose].

The composer scripts that drive the binaries are baked into the
image at build time at `/opt/antithesis/test/v1/stub/` —
nothing extra to mount. The Antithesis composer mounts that path
into its execution sandbox automatically when the test runs.

### Validation gate before merging into a scheduled testnet

Per the project's "no broken container on main testnet" rule, a
new testnet that adds asteria-game must dispatch a 1h Antithesis
run via `workflow_dispatch` on the feature branch and confirm
`findings_new ≤ baseline` before merging. The same gate applied
when promoting asteria-game into [`cardano_node_master`][master-pr-128].

### What does the cluster need to look like

The asteria-game player drives `spawnShip` Plutus transactions
that consume + replace the asteria UTxO. The cluster needs:

- At least one block producer that accepts valid Plutus v3
  transactions (the asteria validators are Aiken-compiled to
  PlutusV3).
- A relay reachable via `relay1.example:3001` (N2N) and
  `relay1-state:/state/node.socket` (N2C). Aliasing it under a
  different name requires editing
  `components/asteria-game/composer/stub/parallel_driver_asteria_player.sh`.
- Genesis funds in `utxo-keys/genesis.1.skey` sufficient for the
  one-time bootstrap deploy plus a few thousand `spawnShip`
  transactions over a 3h run.

The master testnet meets all three by construction. New testnets
that change the topology, network magic, or genesis layout will
need to mirror those choices.

## What it does

The container ships two binaries:

- **`asteria-bootstrap`** — one-shot. Deploys the four asteria validators
  as inline reference scripts, mints the admin NFT, locks the asteria
  UTxO at the asteria spend address with inline `AsteriaDatum`, and
  spawns the initial pellet UTxOs at known coordinates.
- **`asteria-player`** — long-running. Reads game state via the N2C
  local socket, picks an action (`mint_ship`, `move_ship`,
  `gather_fuel`, `mine_asteria`, `quit`) using randomness sourced from
  the antithesis hypervisor when available, submits the resulting tx,
  sleeps a randomly-chosen interval, repeats.

Composer scripts under `/opt/antithesis/test/v1/asteria/`:

- `parallel_driver_asteria_bootstrap.sh` — fires once at start.
- `parallel_driver_asteria_player.sh` — long-running player loop, one
  per replica.
- `eventually_asteria_alive.sh` — confirms the bootstrap event landed
  in the SDK fallback file within a bounded window.
- `helper_sdk_lib.sh` — copy of the shared shell SDK helper.

## Iterations

1. **Wiring proof** *(current).* Both binaries emit `sdk_reachable`
   events to the JSONL fallback file and exit / sleep. No game logic
   yet; this iteration only proves the Nix → docker image →
   docker-compose → composer driver → SDK fallback pipeline is wired
   correctly. See [PR #57][pr-57].
2. **`cardano-node-clients` integration.** Pin
   [`cardano-node-clients@f6a31ca`][cnc-fix] (closes the ref-script
   fee bug that asteria's `move_ship` ref-input path needs) and
   transitive `source-repository-package` dependencies. Player
   connects to N2C and queries protocol parameters.
3. **Real validators.** Compile [txpipe/asteria][asteria-onchain]'s
   four Aiken validators (`asteria.spend`, `spacetime.spend`,
   `pellet.spend`, `deploy.spend`) plus the two minting policies
   (`shipyard.mint`, `fuel.mint`) with parameter application. Commit
   compiled bytes alongside the Aiken sources for reproducibility.
4. **Real bootstrap.** Replace the `sdk_reachable` placeholder with
   the actual deployment tx.
5. **Game loop.** `mintShip` / `moveShip` / `gatherFuel` /
   `mineAsteria` driven by an injectable `RandomSource`; pure
   library functions over `Provider` + `Submitter` from
   `cardano-node-clients`.
6. **Antithesis randomness.** Wire `RandomSource` to
   `antithesis.random.get_random()` (Python subprocess wrapper;
   `System.Random` fallback when the SDK isn't reachable). The
   hypervisor controls every game decision.
7. **Richer assertions.** `eventually_ship_counter_grew`,
   `finally_someone_reached_origin`, `sdk_always(no_double_spend)`,
   per-tx `sdk_sometimes` markers for unusual states.

## Build the image

### Nix

```bash
cd components/asteria-player
nix build .#docker-image
docker load < result
version=$(nix eval --raw .#version)
docker tag \
    ghcr.io/cardano-foundation/cardano-node-antithesis/asteria-player:$version \
    ghcr.io/cardano-foundation/cardano-node-antithesis/asteria-player:dev
```

`just load-docker-image` wraps the same steps.

## Run locally

```bash
INTERNAL_NETWORK=false docker compose \
    -f testnets/cardano_node_master/docker-compose.yaml up -d
```

The compose file declares:

- `asteria-bootstrap` — depends on `relay1`, exits 0 once the
  bootstrap event is written.
- `asteria-player-1` / `asteria-player-2` — depend on
  `asteria-bootstrap` completing successfully; tail forever once
  started.
- `asteria-sdk` named volume — shared between all three so the
  JSONL fallback file (`/sdk/sdk.jsonl`) is observable from any of
  them.

Inspect the JSONL fallback:

```bash
docker run --rm \
    -v cardano_node_master_asteria-sdk:/sdk:ro \
    --entrypoint /bin/cat \
    ghcr.io/cardano-foundation/cardano-node-antithesis/asteria-player:dev \
    /sdk/sdk.jsonl
```

## See Also

- License: see LICENSE
- Contributing: see CONTRIBUTING.md
- Other projects by [HAL][HAL]
- Other projects by the [Cardano Foundation][CF]
- About [Cardano][Cardano]

<!-- MARKDOWN LINKS & IMAGES -->

[asteria]: https://github.com/txpipe/asteria
[asteria-onchain]: https://github.com/txpipe/asteria/tree/main/onchain
[issue-56]: https://github.com/cardano-foundation/cardano-node-antithesis/issues/56
[pr-57]: https://github.com/cardano-foundation/cardano-node-antithesis/pull/57
[cnc-fix]: https://github.com/lambdasistemi/cardano-node-clients/commit/f6a31ca6c169810a46e45648a0868d7a48eb1f02
[master-why]: ../testnets/cardano-node-master.md#why-asteria-game-is-here
[master-compose]: https://github.com/cardano-foundation/cardano-node-antithesis/blob/main/testnets/cardano_node_master/docker-compose.yaml
[master-pr-128]: https://github.com/cardano-foundation/cardano-node-antithesis/pull/128
[HAL]: https://github.com/cardano-foundation/hal
[CF]: https://github.com/cardano-foundation
[Cardano]: https://cardano.org/
