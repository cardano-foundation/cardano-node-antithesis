# Adversary

A Cardano node that talks Node-to-Node protocol and can be used to inject more chaos into Antithesis testing of Cardano network.

> [!CAUTION]
> This tool is intended for testing purpose only, it purposefully bypass safeguards that are normally built into Cardano network clients.
> Never use this on real network!

## Installation

### Without nix

Prerequisites:

* Ensure you have installed a proper [GHC](https://www.haskell.org/ghcup) toolchain, version 9.6.6 or above
* Ensure you have installed system dependencies, checkout [Building cardano-node](https://developers.cardano.org/docs/get-started/infrastructure/node/installing-cardano-node/) guide

```bash
cabal install --overwrite-policy always
```

This should install the binary `adversary` into `$HOME/.cabal/bin` (or whatever `installdir` is configured in your environment).

## Usage

### Transaction Submission

The `adversary` can be used to submit transactions to some node in its network using the [TxSubmission](https://cardano-scaling.github.io/cardano-blueprint/network/node-to-node/txsubmission2/index.html) protocol. To start transaction submission, run something like:

```bash
adversary submit 42 localhost 3001 tx txDir
```

The meaning of each positional argument is the following:

* `42`: this is the network id,
* `localhost` and `3001` are respectively the remote peer's address/name and port,
* remaining arguments (`tx` and `txDir` in this case) are either transaction files or directories.

The `submit` command's behaviour for files and directories arguments are different:
* files are read immediately and submitted to internal transaction submission queue for submission to remote peer,
* directories are _watched_: whenever a new file is written there, it is read and submitted to remote peer.

Transaction files are expected to be the hex-encoded representation of Conway transactions in binary format. The content of these files can be extracted easily from the output of `cardano-cli latest transaction build|build-raw` :

```
cardano-cli latest transaction build ... --out-file tx.raw
cardano-cli latest transaction sign ... --tx-file tx.raw --out-file tx.signed
jq -jc .cborHex tx.signed > rawtx
```

## Command-Line Interface

The `adversary` tool uses sub-commands with named options. Both long options (e.g., `--network-magic`) and short options (e.g., `-m`) are supported.

### Getting Help

```bash
adversary --help                  # Show main help
adversary chainsync --help        # Show chainsync sub-command help
adversary submit --help           # Show submit sub-command help
```

### ChainSync Sub-Command

Connect to Cardano nodes and sync blocks using the ChainSync protocol.

**Example with long options:**
```bash
adversary chainsync \
  --network-magic 42 \
  --port 3001 \
  --sync-length 100 \
  --chain-points-file points.json \
  --num-connections 5 \
  node1.local node2.local node3.local
```

**Example with short options:**
```bash
adversary chainsync -m 42 -p 3001 -l 100 -f points.json -n 5 node1.local node2.local
```

**Options:**
- `-m, --network-magic MAGIC` - Network magic number (e.g., 42 for testnet, 764824073 for mainnet)
- `-p, --port PORT` - Port number to connect to
- `-l, --sync-length LENGTH` - Maximum number of headers to sync
- `-f, --chain-points-file FILE` - File containing chain points (one per line: `hash@slot` or `origin`)
- `-n, --num-connections NUM` - Number of simultaneous connections to open
- `HOSTS...` - List of hosts to connect to (at least one required)

### Submit Sub-Command

Submit transactions to a node using the TxSubmission protocol.

**Example with long options:**
```bash
adversary submit \
  --network-magic 764824073 \
  --host mainnet.cardano.org \
  --port 3001 \
  tx1.hex tx2.hex txDir/
```

**Example with short options:**
```bash
adversary submit -m 764824073 -h mainnet.cardano.org -p 3001 tx1.hex tx2.hex txDir/
```

**Options:**
- `-m, --network-magic MAGIC` - Network magic number (e.g., 42 for testnet, 764824073 for mainnet)
- `-h, --host HOST` - Host to connect to
- `-p, --port PORT` - Port number to connect to
- `TX-FILES...` - Transaction files or directories to monitor (at least one required)
