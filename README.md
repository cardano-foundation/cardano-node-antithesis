![](/docs/static/img/tartarus.jpeg)

# Cardano Node Tests on Antithesis

[![adversary CI](https://github.com/cardano-foundation/cardano-node-antithesis/actions/workflows/adversary-CI.yaml/badge.svg)](https://github.com/cardano-foundation/cardano-node-antithesis/actions/workflows/adversary-CI.yaml) [![Push Cardano Node Antithesis Images](https://github.com/cardano-foundation/cardano-node-antithesis/actions/workflows/publish-images.yaml/badge.svg)](https://github.com/cardano-foundation/cardano-node-antithesis/actions/workflows/publish-images.yaml) [![Test cardano-node](https://github.com/cardano-foundation/cardano-node-antithesis/actions/workflows/cardano-node.yaml/badge.svg)](https://github.com/cardano-foundation/cardano-node-antithesis/actions/workflows/cardano-node.yaml) [![tracer-sidecar CI](https://github.com/cardano-foundation/cardano-node-antithesis/actions/workflows/tracer-sidecar-CI.yaml/badge.svg)](https://github.com/cardano-foundation/cardano-node-antithesis/actions/workflows/tracer-sidecar-CI.yaml)

This repository contains material to test a cardano network of cardano nodes on Antithesis platform.

Current testnets:

- `cardano_node_master`: mixed-version cardano-node network with relays, sidecars, and transaction generation.
- `cardano_amaru`: cardano-node 10.7.1 network with the published `amaru-bootstrap-producer` image and two relay-only Amaru nodes gated on the produced bootstrap bundle. Amaru receives no stake assignment or producer credentials.
- `cardano_amaru_epoch3600`: Antithesis-only variant of `cardano_amaru` with 3600-slot epochs. It is for one-hour Antithesis campaigns where simulated time is long enough for two complete epochs; it is not part of the default wall-clock smoke-test matrix.

It is maintained by the Cardano Foundation as part of the [Moog project][Moog].

Documentation available at [Documentation][Docs]


## See Also

- Other projects by [HAL][HAL]
- Other projects by the [Cardano Foundation][CF]
- About [Cardano][Cardano]

<!-- MARKDOWN LINKS & IMAGES -->

[Docs]: https://cardano-foundation.github.io/cardano-node-antithesis
[Moog]: https://cardano-foundation.github.io/moog
[Antithesis]: https://antithesis.com
[HAL]: https://github.com/cardano-foundation/hal
[CF]: https://github.com/cardano-foundation
[Cardano]: https://cardano.org/
