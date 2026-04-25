# Asteria Player

Long-running container that plays the [asteria][asteria] game inside the
cardano-node-antithesis cluster. Drives realistic transaction traffic
(spend script, mint, reference inputs, validity bounds, inline datums)
through the cluster nodes so antithesis can fuzz state-space exploration
against contention between players.

This component is part of the phase-1 gatherer feature ([#56][issue-56]).

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
[HAL]: https://github.com/cardano-foundation/hal
[CF]: https://github.com/cardano-foundation
[Cardano]: https://cardano.org/
