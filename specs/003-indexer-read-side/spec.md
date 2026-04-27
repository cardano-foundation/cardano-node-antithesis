# Feature Specification: Indexer-Backed Read Side for Test Workloads

**Feature Branch**: `003-indexer-read-side`
**Created**: 2026-04-26
**Status**: Draft
**Input**: "Replace tx-generator's direct LSQ queries (`cardano-cli query utxo --address`) with reads against a small in-memory indexer daemon running alongside the cluster. Fix the lossy self-transfer dust bug via a deterministic tx chain plus a refund actor so the post-submit polling loop disappears. Asteria-player (PR #67) consumes the same indexer."
**Parent issue**: [cardano-foundation/cardano-node-antithesis#69](https://github.com/cardano-foundation/cardano-node-antithesis/issues/69)
**Related**:
- [cardano-foundation/cardano-node-antithesis PR #67](https://github.com/cardano-foundation/cardano-node-antithesis/pull/67) — asteria-spawn-v2 parked on the same axis.
- [lambdasistemi/cardano-node-clients#78](https://github.com/lambdasistemi/cardano-node-clients/issues/78) — minimal address→UTxO indexer daemon (Unix socket, NDJSON, in-memory). This spec consumes it.
- Rejected upstream candidate (and why): [`lambdasistemi/cardano-utxo-csmt`](https://github.com/lambdasistemi/cardano-utxo-csmt) and the closed transport ticket [`#233`](https://github.com/lambdasistemi/cardano-utxo-csmt/issues/233). That daemon's structure (CSMT column, `Restoring`/`Full`/`KVOnly` dual-mode backend, journal-replay, proof endpoints) exists to commit the UTxO set into a merkle tree and serve inclusion proofs — none of which this spec needs.

## Clarifications

### Session 2026-04-27

- Q: tx-generator chain length & refund model — unbounded, bounded with refund, fixed-N + restart, or splitter-then-loop? → A: Bounded chain with a refund actor — when the active UTxO falls below threshold T, tx-generator submits a refund tx that consumes the active UTxO together with a faucet UTxO (held by the same key) and produces a single replenished active UTxO above T.
- Q: Build a new indexer or reuse `cardano-utxo-csmt`? → A: Build a new minimal daemon. `cardano-utxo-csmt`'s structure is built around CSMT + inclusion proofs; the dual-mode backend exists because the merkle tree needs different write paths during catch-up vs. live-tip. Without proofs the entire dual-mode disappears, and stripping CSMT leaves roughly nothing reusable. Filed as [`cardano-node-clients#78`](https://github.com/lambdasistemi/cardano-node-clients/issues/78).
- Q: TCP-over-bridge or Unix-socket-over-shared-volume for indexer↔consumer IPC? → A: Unix domain socket on a shared volume. Matches the existing compose pattern (every service in this compose reads chain state via a Unix socket mounted from another service's volume); narrower deterministic-replay surface under Antithesis.
- Q: tx confirmation primitive? → A: Wait for `(txId, ix)` to appear in the index — confirmation reduces to "wait for any output of my submitted tx to land". Single primitive, point-equality predicate, no separate tx-confirmation table inside the daemon.
- Q: asteria-player's policy/asset queries? → A: For v1, address-keyed lookup + in-process filtering on `value`. Ships, pellets, and the central Asteria UTxO all live at well-known addresses. A policy-keyed endpoint becomes a follow-up only if profiling shows in-process filtering is too slow.
- Q: Persistence model? → A: In-memory only, re-index from genesis on every restart. Devnet chain is short (seconds). If Antithesis fault-injection times prove unacceptable, persistence is a bounded follow-up on the daemon — one-line backend swap.

## User Scenarios & Testing *(mandatory)*

### User Story 1 — tx-generator runs sustained workload without saturating the relay's LSQ channel (Priority: P1)

A fresh local cluster is up. tx-generator submits transactions continuously. The read side (sender's UTxO discovery, faucet UTxO discovery on refund) is served by the indexer daemon ([cardano-node-clients#78](https://github.com/lambdasistemi/cardano-node-clients/issues/78)) running alongside the cluster, reachable over a Unix domain socket on a shared volume. The relay's `LocalStateQuery` channel sees no `GetUTxOByAddress` traffic from tx-generator, and submission continues at the configured cadence with no `Tx failed` / `UTxO still present after 120s` lines in tx-generator logs over a 10-minute window.

**Why this priority**: this is the load-bearing change. Without it every Antithesis result is contaminated — "node fell over from query saturation" is indistinguishable from "node fell over from injected fault". Until the read path is moved off LSQ-by-address, the test signal is meaningless under sustained load.

**Independent Test**: bring up `testnets/cardano_node_master` with `INTERNAL_NETWORK=false docker compose up -d`. After 5 minutes: `docker logs tx-generator | grep -cE 'Tx failed|UTxO still present|does not meet'` returns 0, and a packet capture (or LSQ trace) on the relay shows zero `GetUTxOByAddress` requests originating from tx-generator's container.

**Acceptance Scenarios**:

1. **Given** a healthy cluster with the indexer daemon reporting `ready=true`, **When** tx-generator runs for 10 minutes at its configured rate, **Then** every UTxO lookup it performs hits the daemon's `utxos_at` request on the shared-volume Unix socket, no `cardano-cli query utxo --address` invocation appears in tx-generator's process tree or logs, and no `Tx failed` lines appear.
2. **Given** the daemon is reachable but momentarily lagging (i.e. `slotsBehind > 0`), **When** tx-generator asks for the sender's UTxOs, **Then** it receives the daemon's last-applied set, decides whether to wait or proceed deterministically (see User Story 2), and never falls back to LSQ-by-address against the node.
3. **Given** the daemon's Unix socket is unreachable for K seconds, **When** tx-generator asks for UTxOs, **Then** it surfaces a clear error and stops attempting submissions until the daemon recovers — it MUST NOT silently fall back to querying the node.

---

### User Story 2 — tx-generator chains self-spends deterministically, refunds before dust, never wedges (Priority: P1)

tx-generator picks an initial active UTxO once (via `utxos_at` on the daemon at startup) and runs a self-pay chain off it. For each subsequent self-pay transaction, the next `TxIn` is computed from the just-built tx body (the new `TxId` plus the known output index) — no UTxO re-query, no post-submit polling. When the active UTxO's value falls below a configured refund threshold T (T > min-UTxO + safety margin), tx-generator builds a refund transaction that consumes the active UTxO plus a faucet UTxO (held under the same key, discovered via the daemon at startup or on demand) and produces a single replenished active UTxO above T. The chain continues off the refund tx's output. After K successive self-spends spanning multiple refunds, tx-generator is still alive, still submitting, and the active UTxO is still well above the min-UTxO floor.

Optionally, tx-generator confirms each submitted tx via `await` on the daemon for the self-pay output's `TxIn`. The deterministic chain does not require this (the next tx can build on the unconfirmed `TxIn`), but `await` is available when stricter pacing is wanted (e.g. an outer Antithesis driver gating on confirmation).

**Why this priority**: the dust-drain bug compounds the LSQ problem (more "looking for a non-dust UTxO" means more LSQ scans) and is the proximate cause of the wedge today. The deterministic-chain pattern eliminates both the post-submit polling loop and the dust drift in one stroke; the refund actor keeps the workload long-running.

**Independent Test**: run tx-generator for 30 minutes against a healthy local cluster, then `docker logs tx-generator | grep -cE 'does not meet the minimum UTxO threshold|UTxO still present'` returns 0; the count of submitted txs (self-pay + refund) roughly equals `elapsed / configured_interval`.

**Acceptance Scenarios**:

1. **Given** a starting active UTxO of value V well above threshold T and a self-pay chain of length N, **When** tx-generator runs the chain for N submissions, **Then** every self-pay tx uses a `TxIn` derived from the previous tx body without any UTxO query, and the active UTxO's value is at least min-UTxO + safety margin at every step (refund txs intercalated as needed).
2. **Given** the active UTxO's value drops below threshold T mid-chain, **When** tx-generator builds the next tx, **Then** it instead builds a refund tx that consumes the active UTxO plus a faucet UTxO (same key) and produces a single replenished active UTxO above T; the next self-pay tx is built off that refund's output without re-querying.
3. **Given** a submission failure mid-chain (e.g. node rejects a tx), **When** tx-generator detects the failure, **Then** it re-syncs its known UTxO set via `utxos_at` and resumes the chain from the actual on-chain state.
4. **Given** the post-submit "wait until consumed" loop in today's `tx_generator.py`, **When** the new chain logic ships, **Then** that loop is removed entirely — there is no remaining code path that re-queries the node to confirm consumption. Where waiting is wanted, it goes via the daemon's `await`.
5. **Given** no faucet UTxO is reachable when refund is required, **When** tx-generator needs to refund, **Then** it surfaces a clear error and stops submitting; it MUST NOT silently let the active UTxO drift to dust.

---

### User Story 3 — Asteria-player consumes the same daemon for its read side (Priority: P1)

The asteria-player workload (currently parked on PR [#67](https://github.com/cardano-foundation/cardano-node-antithesis/pull/67)) discovers ship UTxOs, pellet UTxOs, and the central Asteria UTxO via the same indexer daemon used by tx-generator. For v1 the lookups are address-scoped: shipyard address, pellet address (spacetime), asteria address — each fetched via `utxos_at` and filtered in-process on `value` to pick out the specific ship token / pellet / admin NFT. No `cardano-cli query utxo --address` calls remain in asteria-player or in any helper it shells out to. PR [#67](https://github.com/cardano-foundation/cardano-node-antithesis/pull/67) can be unparked and merged once this dependency lands.

**Why this priority**: the issue and PR are explicitly coupled — "asteria off-chain code uses the same `queryUTxOs` LSQ pattern, and we agreed it has to stop" ([#69](https://github.com/cardano-foundation/cardano-node-antithesis/issues/69)). Shipping the daemon for tx-generator alone leaves asteria as a second LSQ saturator and PR [#67](https://github.com/cardano-foundation/cardano-node-antithesis/pull/67) still blocked.

**Independent Test**: with the daemon running, execute `parallel_driver_asteria_gatherer.sh` (or the equivalent in PR [#67](https://github.com/cardano-foundation/cardano-node-antithesis/pull/67)'s tree) against a healthy cluster; the gatherer completes a move-and-gather cycle without invoking `cardano-cli query utxo --address` anywhere in its process tree, and the relay's LSQ trace shows zero address-scoped UTxO queries for the duration.

**Acceptance Scenarios**:

1. **Given** the daemon is up and asteria bootstrap has completed, **When** a gatherer replica runs one cycle, **Then** {ship UTxO lookup, nearest-pellet discovery, central Asteria UTxO read} all resolve via `utxos_at` calls on the daemon's Unix socket; zero LSQ-by-address requests reach the relay from the asteria-player container.
2. **Given** PR [#67](https://github.com/cardano-foundation/cardano-node-antithesis/pull/67)'s branch (`asteria-spawn-v2`) rebased onto the daemon-bearing main, **When** the test workload runs end-to-end, **Then** acceptance for that PR's own user stories still passes and the LSQ-saturation regression observed today does not recur.

---

### User Story 4 — Indexer daemon is part of the standard testnet compose topology (Priority: P2)

The indexer daemon runs as a regular service in `testnets/cardano_node_master/docker-compose.yaml` (and any sibling testnet compose files actually exercised). It mounts one relay's state volume read-only (so it can talk to the relay's Unix socket via N2C chain-sync), holds its index in memory, and listens on a Unix socket placed on a shared `indexer-sock` volume. tx-generator and asteria-player mount that shared volume read-only and connect to the socket. The daemon's `ready` request is wired into a compose healthcheck so dependent services do not proceed until ready.

**Why this priority**: needed for reproducibility and for Antithesis runs (which use the same compose topology). Not P1 because P1/P2/P3 above could in principle be demonstrated with an out-of-band daemon for the first acceptance pass.

**Independent Test**: `docker compose up -d` brings the daemon up alongside the cluster, `docker compose ps` reports it healthy within a documented bound, and a test client from a sibling container connecting to the shared-volume Unix socket gets a well-formed UTxO list for a known funded address.

**Acceptance Scenarios**:

1. **Given** a clean checkout, **When** a developer runs `INTERNAL_NETWORK=false docker compose up -d`, **Then** the daemon container starts, becomes healthy (`ready=true`) without manual intervention, and is reachable by tx-generator and asteria-player via the shared-volume Unix socket.
2. **Given** the daemon container is killed mid-run (Antithesis fault injection), **When** docker compose restarts it, **Then** it re-indexes from genesis (devnet chain is short — seconds), reaches `ready=true` again, and tx-generator / asteria-player resume reads without each needing a restart.

---

### User Story 5 — Tip queries are minimised and clearly justified (Priority: P3)

Any remaining `cardano-cli query tip` call (or equivalent slot/era query) is either: (a) removed because the tx pattern doesn't actually need tip; or (b) replaced by the daemon's `ready` request (which already returns `tipSlot` and `processedSlot`); or (c) retained with a one-line justification next to it. No code path uses tip as a stand-in for "wait until my tx lands" — that reduces to the daemon's `await`.

**Why this priority**: tip queries are constant-time and not the saturator, so this is hygiene rather than a load issue. Worth doing in the same pass to avoid re-introducing an LSQ habit.

**Independent Test**: `grep -n 'query tip' components/tx-generator components/asteria-player` returns either zero hits or hits each accompanied by a justification comment.

**Acceptance Scenarios**:

1. **Given** the new code paths, **When** the read side runs end-to-end, **Then** every retained `query tip` site has an inline justification, no `query tip` call gates "tx was consumed" logic, and where consumers needed slot/tip information they now read it from the daemon's `ready` or use `await`.

---

### Edge Cases

- **Daemon cold start**: re-indexes from genesis on every restart. `ready` returns `false` with `slotsBehind > 0` until caught up; dependent services gate on `ready=true`. On devnet this is seconds.
- **Rollback handling**: chain-sync rollback events are applied by the daemon. tx-generator's deterministic chain may have used a `TxIn` that gets rolled back; on submission failure or on a snapshot mismatch the consumer re-syncs via `utxos_at` (User Story 2 scenario 3). Pending `await` waiters whose `TxIn` was observed and then rolled back stay pending.
- **Concurrent consumers**: tx-generator and asteria-player connect concurrently. The daemon must accept multiple Unix-socket clients in parallel; each long-lived `await` connection is independent and must not block snapshot/ready calls.
- **Authorization**: socket file lives on a shared docker volume; filesystem permissions are the only access control. No TCP exposure to host or external networks.
- **Submission failures and re-orgs**: when a submitted tx is rejected or re-orged out, the deterministic chain assumption is broken. Recovery: re-sync via `utxos_at`, re-pick the active `TxIn`, continue.
- **Daemon container killed during Antithesis fault injection**: consumers see EOF / connection refused on the Unix socket; they MUST NOT fall back to direct node queries — they pause submission, retry the connection, resume once `ready=true` again.
- **Faucet exhaustion**: refund tx requires a faucet UTxO of sufficient value; if none is available, tx-generator surfaces a clear error and stops (User Story 2 scenario 5).
- **Daemon process crash**: with no persistence, restart loses the in-memory index; recovery is "re-index from genesis", which is acceptable on devnet.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: tx-generator MUST obtain the sender's UTxO set exclusively from the indexer daemon (`utxos_at` over its Unix socket). It MUST NOT invoke `cardano-cli query utxo --address` (or any equivalent `LocalStateQuery GetUTxOByAddress` shell-out) under any code path, including error and retry paths.
- **FR-002**: Asteria-player (and any helper it invokes) MUST obtain on-chain read state (ship UTxOs, pellet UTxOs, central Asteria UTxO, admin NFT) exclusively from the daemon's `utxos_at` request, with the same prohibition on direct LSQ-by-address calls. v1 uses in-process filtering on `value` for asset-class discovery.
- **FR-003**: tx-generator MUST submit transactions as a deterministic chain: each subsequent self-pay transaction's input is computed from the previous transaction's body (TxId + known output index), without any UTxO re-query and without any post-submit polling loop. Where confirmation waiting is wanted, it goes via the daemon's `await`.
- **FR-004**: tx-generator MUST keep the active UTxO above a configured refund threshold T (with T > min-UTxO + safety margin) by submitting refund transactions that consume the active UTxO together with a faucet UTxO (held under the same key) and produce a single replenished active UTxO above T. The chain MUST continue off the refund transaction's output without any UTxO re-query.
- **FR-004a**: tx-generator MUST discover the faucet UTxO(s) exclusively via the daemon's `utxos_at` request (at startup, and when needed for a refund). It MUST NOT shell out to `cardano-cli query utxo --address` to find the faucet.
- **FR-004b**: When a refund is required but no faucet UTxO is available via the daemon, tx-generator MUST surface a clear error and stop submitting; it MUST NOT silently allow the active UTxO to drift to dust or fall back to direct node queries.
- **FR-005**: tx-generator MUST remove the existing post-submit "UTxO still present" polling loop from `tx_generator.py`; it is incompatible with FR-003 and is a contributor to LSQ load.
- **FR-006**: tx-generator and asteria-player MUST handle a momentarily-lagging or unreachable daemon by surfacing a clear error and pausing submission, NOT by falling back to direct node queries.
- **FR-007**: The indexer daemon MUST be added to the testnet docker compose topology, MUST mount one relay's state volume read-only (for N2C chain-sync against the relay's Unix socket), and MUST listen on a Unix domain socket placed on a shared volume that consumers mount read-only.
- **FR-008**: The daemon's `ready` request MUST be wired into a compose healthcheck (or equivalent gating) so dependent services do not begin reads until `ready=true`.
- **FR-009**: Transaction submission itself MUST remain on the LocalTxSubmission mini-protocol via the existing relay socket; this change does not move submission off the node.
- **FR-010**: Any retained `cardano-cli query tip` (or equivalent slot/era query) MUST be either removed, replaced by the daemon's `ready` request, or accompanied by an inline justification; tip MUST NOT be used as a proxy for "my tx was consumed" — that reduces to the daemon's `await`.
- **FR-011**: The read-side path MUST tolerate chain rollbacks: on a submission failure or a snapshot mismatch the consumer MUST re-sync via `utxos_at` rather than failing hard. Rollback handling within the daemon is the daemon's responsibility, per [cardano-node-clients#78](https://github.com/lambdasistemi/cardano-node-clients/issues/78).
- **FR-012**: This spec depends on [cardano-node-clients#78](https://github.com/lambdasistemi/cardano-node-clients/issues/78) (the daemon itself). The compose wiring lands once the daemon ships.

### Key Entities *(include if feature involves data)*

- **Indexer daemon**: A small in-memory service (proposed as [cardano-node-clients#78](https://github.com/lambdasistemi/cardano-node-clients/issues/78)) that follows the chain via N2C from one relay socket and maintains an address→UTxO index. Two read primitives — `utxos_at` (snapshot) and `await` (block until `TxIn` appears) — plus a `ready` request, all over a Unix domain socket using NDJSON. No persistence, no proofs, no merkle tree, no dual-mode backend.
- **Sender UTxO chain (tx-generator)**: A deterministic sequence of self-pay UTxOs where the next input is computed from the previous tx body. Includes an *active UTxO* (the head of the chain), a configured *refund threshold T* below which a *refund transaction* fires, and one or more *faucet UTxO(s)* held under the same key that supply lovelace into refund transactions.
- **Asteria read view**: The set of on-chain entities the asteria workload must observe — ship UTxOs (per shipyard policy token), pellet UTxOs at the spacetime address, the singleton Asteria UTxO at the asteria address, the admin NFT. v1 reads each via `utxos_at` on the relevant address and filters in-process on `value`.
- **Compose topology**: The docker compose configuration that wires relay → daemon (relay-state volume mount, read-only, for N2C chain-sync) and daemon → consumers (shared `indexer-sock` volume holding the Unix socket file).

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: After 30 minutes of sustained tx-generator operation against a healthy local cluster, `docker logs tx-generator | grep -cE 'Tx failed|UTxO still present|does not meet the minimum UTxO threshold'` returns 0 (today: 23 in 5 minutes per [#69](https://github.com/cardano-foundation/cardano-node-antithesis/issues/69)).
- **SC-002**: An LSQ trace on the relay over a 10-minute representative workload shows zero `GetUTxOByAddress` requests originating from tx-generator's or asteria-player's containers.
- **SC-003**: tx-generator's submitted-tx count over a fixed window matches `elapsed / configured_interval` within ±5% (i.e. it is no longer wedging or stalling on UTxO discovery), counting self-pay and refund txs together.
- **SC-004**: Antithesis runs of duration ≥ 1 hour against the cluster do not produce findings whose root cause is read-side query saturation; any node-fell-over findings can be attributed to injected faults.
- **SC-005**: PR [#67](https://github.com/cardano-foundation/cardano-node-antithesis/pull/67) (asteria-spawn-v2) can be rebased and merged on top of the daemon landing; its acceptance for User Story 1 (one move-and-gather cycle) passes end-to-end without LSQ-by-address calls from the asteria-player container.
- **SC-006**: A clean `docker compose up -d` brings the daemon to `ready=true` within a documented bound (target: under 1 minute on the local devnet); dependent services start reads only after that point.
- **SC-007**: After a forced restart of the daemon container (e.g. `docker compose restart`), `ready=true` returns within the same bound as cold start (re-indexing from genesis is acceptable at devnet scale).

## Assumptions

- The daemon scoped in [cardano-node-clients#78](https://github.com/lambdasistemi/cardano-node-clients/issues/78) ships before the compose wiring in this spec is finalised. Until then, the read-side rewrite of tx-generator and asteria-player can proceed against an out-of-band test fixture exposing the same Unix-socket NDJSON contract.
- The relay's chain-sync mini-protocol can serve the daemon's follower in addition to the relay's existing N2N peers. (If it doesn't, that is a separate node-level finding, not a scope expansion of this spec.)
- tx-generator's current behaviour (self-pay chain) is the intended workload shape; this spec changes how UTxOs are discovered and how the chain is constructed (deterministic chain + refund actor), not the high-level "submit lots of cheap txs" goal.
- Asteria-player's read pattern (address-scoped UTxO lookup with in-process filter on `value`) is sufficient for v1. Adding a policy-keyed endpoint to the daemon is a follow-up only if profiling shows the in-process filter is too slow.
- The fix lands first as a single PR on `cardano-node-antithesis`'s `main` branch (this spec); the asteria-spawn-v2 rebase ([PR #67](https://github.com/cardano-foundation/cardano-node-antithesis/pull/67)) is the second step, not parallel.
- Submission via LocalTxSubmission is unchanged and remains acceptable, per [#69](https://github.com/cardano-foundation/cardano-node-antithesis/issues/69).
- Existing testnet compose files (`testnets/cardano_node_master/docker-compose.yaml` and any actively used siblings) are the integration surface; obsolete or experimental compose variants are out of scope.
