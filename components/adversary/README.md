# Adversarial node for Cardano-Antithesis

The idea is to create an executable which:

- can be triggered by Antithesis
- behaves like a node but with unusual (possible adversarial) behaviours
- in order to test the "real" nodes in the cluster run by Antithesis

Cardano protocol details:

- Node to node protocol (we act like a node)
- Specifically chain sync mini-protocol (p.21 of [Network-spec][Netspec]), peer
  propagation protocol.

- As a downstream peer:
  - Unsual behaviours:
    * ask to sync to random intersection points (for example, blocks that might have been rolled back)
    * multiple random connect/follow/disconnect (maybe use Antithesis for randomness?)
- As an upstream peer:
  - serve incorrect headers/blocks/chains.

Overall approach:

1. Using tracer sidecar, get all intersection points from logs of nodes in cluster
2. Write these chain points into a file on disk
3. Adversarial node reads this file selects random points
4. Provide a command (to let Antithesis decide when to use it)
5. When command is called, select random point from file and do a chain sync

Implementation considerations:

- Make a Haskell executable
- Not absurd to start from scratch but can look at cardano-wallet code for inspiration
- Must have [ouroboros-network][Ouroboros] (and possibly ouroboros-consensus-cardano) as dependencies
- Might need external C libraries (libsodium)
- All this might make it necessary to use Nix to get all dependencies working


TODO:
- context
- what it does
- who is it for
- why one would use it

Any details requiring more than three short paragraphs should go in a separate document (typically a white paper).

## Pre-requisites

Describe pre-requisites, in particular:

- skills, experience level
- deployment (browser, mobile, desktop, cloud...)
- operating system
- tools, runtimes (e.g. node.js, nix, ...)

## Getting started

How to:

- download or clone
- install
- start or run

## Usage

A few basic examples of how to use (not a complete reference).

## See Also

- link(s) to more detailed project documents
- License: see LICENSE
- Contributing: see CONTRIBUTING.md
- Security: see SECURITY.md
- Other projects by [HAL][HAL]
- Other projects by the [Cardano Foundation][CF]
- About [Cardano][Cardano]

<!-- MARKDOWN LINKS & IMAGES -->

[Ouroboros]: https://github.com/IntersectMBO/ouroboros-network
[Netspec]: https://ouroboros-network.cardano.intersectmbo.org/pdfs/network-spec/network-spec.pdf
[HAL]: https://github.com/cardano-foundation/hal
[CF]: https://github.com/cardano-foundation
[Cardano]: https://cardano.org/

