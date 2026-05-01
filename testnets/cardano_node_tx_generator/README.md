# cardano_node_tx_generator

Feature testnet for iterating on the `cardano-tx-generator` daemon.

Same cluster topology as `cardano_node_master` (3 producers + 2 relays
+ tracer + sidecars + log-tailer) plus the `tx-generator` service that
drives N2C tx-submission load against `relay1`. Asteria-stub is
deliberately absent here — it lives on the master testnet because it
exercises a different feature (long-lived utxo-indexer).

## Why a separate testnet

The master testnet is the canonical reference cluster: changes there
affect the cron-fired Antithesis run and everyone reading its
findings. Tx-generator is in active iteration (reconnect supervisor,
freshness gate, composer-assertion shape) and its experiments would
otherwise pollute master's signal. Dispatch this testnet via
workflow_dispatch instead:

```sh
gh workflow run cardano-node.yaml \
  --ref <feature-branch> \
  -f test=cardano_node_tx_generator \
  -f duration=1
```

## Image publishing

The `publish-images` workflow currently scrapes images from
`testnets/cardano_node_master/docker-compose.yaml`. Until that's
generalised to all testnets, the tx-generator image referenced in the
compose here is pushed manually — `nix build .#docker-image` from
`components/tx-generator/`, `docker tag` + `docker push`.
