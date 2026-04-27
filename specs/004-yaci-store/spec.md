# Feature Specification: yaci-store as Antithesis N2N consumer

**Feature Branch**: `004-yaci-store`
**Created**: 2026-04-27
**Status**: Draft
**Input**: User description: "Add a yaci-store container to the cardano_node_master testnet as a long-running N2N consumer for protocol coverage under Antithesis fault injection. Template from Oura: N2N TCP to a relay (relay1 or relay2), magic=42, custom byron/shelley genesis windows. Backing store: embedded (H2) — pure coverage, no app reads. Image: pinned digest from bloxbean/yaci-store-spring-boot, re-published via the repo's publish-images workflow. Scope is additive: independent of PR #70's indexer-read-side. Refs #75."
**Issue**: [#75](https://github.com/cardano-foundation/cardano-node-antithesis/issues/75)

## User Scenarios & Testing *(mandatory)*

### User Story 1 - yaci-store joins the testnet and tracks the chain (Priority: P1)

As a test engineer running the Antithesis testnet, I want yaci-store to come up alongside the existing services, connect to a relay over node-to-node, and follow the chain to the current tip — so that the testnet exercises a real-world N2N consumer profile alongside Oura.

**Why this priority**: Without P1, the feature delivers no coverage. yaci-store is the most-used Java/Spring-Boot indexer in the Cardano ecosystem; running it under fault injection is the entire point of the ticket.

**Independent Test**: `just up cardano_node_master`, wait for nodes to produce blocks (existing smoke test), then verify that yaci-store reports a tip block with slot equal to or greater than the current relay tip within an agreed time budget. No coupling to other services beyond the relay it consumes from.

**Acceptance Scenarios**:

1. **Given** the testnet is brought up cold, **When** all configured nodes have produced at least one block and yaci-store has been running for the configured warm-up window, **Then** yaci-store's reported tip slot equals or exceeds the relay's reported tip slot.
2. **Given** the testnet is running steady-state, **When** the relay yaci-store connects to is restarted, **Then** yaci-store reconnects and resumes following the chain without operator intervention.
3. **Given** a fresh testnet, **When** yaci-store starts before the network has produced any blocks, **Then** it waits and starts indexing once block production begins (no crash on empty chain).

---

### User Story 2 - yaci-store survives Antithesis fault injection without corrupting the test verdict (Priority: P1)

As an Antithesis test author, I want yaci-store to either keep running through fault injection or fail in a way that does not produce false-positive findings on the testnet.

**Why this priority**: Antithesis findings are the product the testnet exists to deliver. A new container that crashes spuriously under SIGTERM/SIGKILL/network partition would make every run noisy and is worse than not having the container at all. yaci-store **may** crash under fault injection — what matters is that the harness is configured to absorb the noise correctly.

**Independent Test**: Run a 1-hour Antithesis duration on this branch. Compare findings against the previous baseline (current `main`): any new finding must be attributable to yaci-store by name, and the count of new findings must be zero, or each new finding must be triaged and either filed as a separate issue or whitelisted in the harness with a reason.

**Acceptance Scenarios**:

1. **Given** a 1-hour Antithesis run on this branch, **When** the report is compared to the prior baseline, **Then** `findings_new ≤ baseline` (constitution hard gate).
2. **Given** yaci-store receives SIGTERM during teardown, **When** the container exits, **Then** the exit code is recorded; if non-zero (cf. #49 for Ogmios/Kupo), the harness does not fail the run on that exit code alone.

---

### User Story 3 - yaci-store's published image is reproducible and reachable from Antithesis (Priority: P2)

As a maintainer landing this on `main`, I want the yaci-store image referenced in `docker-compose.yaml` to be a content-addressed digest pointing at an image that the publish-images workflow has actually mirrored, so that scheduled Antithesis runs do not break with `manifest unknown`.

**Why this priority**: Past breakage in this area is documented (memory: "No `:dev` in compose", commits 4ebc583 / b89004d / 8c54012 pinning every image to its content digest). This is a P2 because if P1/P2 above are met but the image cannot be pulled by Antithesis, the testnet is broken in production.

**Independent Test**: Inspect the `publish-images.sh` log for the commit that lands this feature. The yaci-store entry must appear with a resolved digest, and the digest in `docker-compose.yaml` must match. A `docker pull` of the digest from a fresh machine must succeed.

**Acceptance Scenarios**:

1. **Given** the compose file is on `main`, **When** the publish-images workflow runs, **Then** the yaci-store image is mirrored under the project's GHCR namespace and the compose digest resolves to a real manifest.
2. **Given** the upstream image is on Docker Hub, **When** the compose references it, **Then** the reference uses the `docker.io/` prefix Antithesis requires.

### Edge Cases

- yaci-store starts before any block is produced (cold boot of a private testnet).
- The relay yaci-store consumes from is restarted, killed, or partitioned.
- yaci-store's embedded store hits its disk quota on a long Antithesis run.
- yaci-store receives SIGTERM during compose teardown — JVM shutdown semantics under the harness.
- The custom magic / genesis windows do not match what yaci-store expects (mainnet-only assumptions in the upstream image).

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The testnet MUST include a yaci-store service in `testnets/cardano_node_master/docker-compose.yaml` that comes up with `just up` and is torn down with `just down`.
- **FR-002**: yaci-store MUST consume the chain from a relay over node-to-node (TCP, port 3001 of either `relay1.example` or `relay2.example`).
- **FR-003**: yaci-store MUST be configured for the testnet's protocol parameters (magic 42 and the byron/shelley genesis windows produced by the configurator).
- **FR-004**: yaci-store MUST use an embedded backing store (no separate database container introduced by this feature).
- **FR-005**: The yaci-store image reference in `docker-compose.yaml` MUST be a content-addressed digest, never `:latest` or `:dev`, and MUST be mirrored by the project's publish-images workflow so it is pullable from Antithesis runners.
- **FR-006**: All bind mounts for yaci-store MUST live next to `docker-compose.yaml` (Antithesis cannot resolve relative paths outside the compose directory).
- **FR-007**: yaci-store MUST NOT block compose start-up of any existing service; its `depends_on` MUST be limited to what it actually needs (configurator and the chosen relay).
- **FR-008**: A 1-hour Antithesis duration on the feature branch MUST report `findings_new ≤ baseline` against the prior `main` baseline; any new finding MUST be triaged before merge.
- **FR-009**: The shutdown exit code of yaci-store under the harness MUST be observed and documented; if non-zero on SIGTERM (analogous to #49), the harness MUST be configured to absorb that exit code or yaci-store MUST be reconfigured to exit cleanly.
- **FR-010**: This feature MUST NOT modify the read path of `tx-generator` or `asteria-player`; yaci-store is additive coverage only and is independent of #70.

### Key Entities

- **yaci-store service**: A new long-running container. Inputs: TCP connection to a relay, custom protocol magic, byron/shelley genesis windows. Output: an embedded indexed view of the chain (used for nothing in this feature — pure coverage).
- **yaci-store image digest**: A content-addressed image reference recorded in the compose file and mirrored by the publish-images workflow.
- **yaci-store config artifact**: A configuration file (or environment overrides) bind-mounted into the container, capturing magic + genesis windows. Lives next to `docker-compose.yaml`.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: After `just up cardano_node_master`, yaci-store reaches a tip slot equal to or greater than the relay's tip slot within 5 minutes of the network producing its first block, on a developer laptop.
- **SC-002**: A 1-hour Antithesis run on `004-yaci-store` produces no new findings versus the prior `main` baseline; if any new findings appear, each is triaged and either filed or whitelisted with a documented reason.
- **SC-003**: Cold `docker compose up -d` succeeds and the yaci-store service is `healthy` (or `running` if no healthcheck) within 1 minute on a devnet machine.
- **SC-004**: After `just down`, no yaci-store volumes, networks, or processes are left behind (clean teardown).
- **SC-005**: The yaci-store image digest in `main` is resolvable by `docker pull` from a machine that has never seen the upstream Docker Hub image — i.e., publish-images has mirrored it.

## Assumptions

- yaci-store's upstream image (`bloxbean/yaci-store-spring-boot` or equivalent) supports private testnets with custom protocol magic and custom byron/shelley genesis windows. If it does not, this feature is reduced to a no-op or requires upstream changes — surface in `/speckit.plan`.
- The configurator already emits genesis files into `p?-configs:/configs/configs/`; yaci-store will reuse those, mounted read-only.
- "Pure coverage" means downstream services do not query yaci-store — the feature ships without API consumers and without surface area to break.
- The publish-images workflow can mirror Docker Hub images to the project's GHCR namespace today; if not, the publish-images change is captured as a separate dependency.
- Antithesis findings whitelisting is in scope of the harness, not this feature; if a yaci-store-specific exit code requires a harness change, that change ships in the same PR.
