# Contributing to cardano-node-antithesis

Thank you for considering to contribute to cardano-node-antithesis and help the Cardano community improve the security of the network!

We especially welcome help improving the documentation and packaging.

The preferred way of contributing is through our [GitHub repository][Repo], by opening issues or submitting pull requests.

Other ways to get in touch are provided on the [cardano-node-antithesis website][Website].

## Repository structure

- `components/`: Reusable Docker containers (configurator, sidecar, tracer-sidecar, adversary, tx-generator)
- `testnets/`: Testnet configurations (docker-compose + genesis parameters)
- `docs/`: Documentation source (MkDocs, published to GitHub Pages)
- `scripts/`: Build and deployment scripts

## Local development

```bash
# Start the testnet locally
just up

# Check container status
just ps

# Stop and clean up
just down
```

## Adding a new component

1. Create `components/<name>/Dockerfile`
2. Reference it in `testnets/cardano_node_master/docker-compose.yaml` as `ghcr.io/cardano-foundation/cardano-node-antithesis/<name>:<commit>`
3. The `publish-images` workflow automatically builds and pushes on PR or merge to main

<!-- MARKDOWN LINKS & IMAGES -->

[Website]: https://cardano-foundation.github.io/cardano-node-antithesis
[Repo]: https://github.com/cardano-foundation/cardano-node-antithesis
