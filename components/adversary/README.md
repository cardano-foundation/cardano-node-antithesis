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
