# Data Model: Indexer-Backed Read Side for Test Workloads

**Feature**: 003-indexer-read-side
**Date**: 2026-04-27
**Spec**: [spec.md](./spec.md) | **Plan**: [plan.md](./plan.md) | **Research**: [research.md](./research.md)

This is a workload port + compose change, not a service with persistent data. The "data model" here is (a) the in-process state that tx-generator carries between iterations of its submit loop, and (b) the wire shapes consumed from the indexer daemon (which are also documented as the contract in [contracts/indexer-client.md](./contracts/indexer-client.md)).

## E1 — `TxGenState` (in-memory, single-threaded)

Held as Python attributes on the running tx-generator process. No persistence; on container restart, recovered by re-reading the address from the indexer (per [research.md R-001](./research.md)).

| Field | Type | Source / Invariant |
|---|---|---|
| `address` | `str` (bech32) | Picked at startup from `/utxo-keys/genesis.<N>.addr.info`; same key for the entire run. |
| `skey_path` | `str` (path) | Sibling `genesis.<N>.skey` to `address`. |
| `active_txin` | `(txid: str, ix: int)` | Initialised at startup from `utxos_at(address)` selecting any UTxO with `lovelace ≥ T`. After each successful self-pay tx, advanced to `(new_txid, 0)`. After each refund tx, advanced to `(refund_txid, 0)`. |
| `active_lovelace` | `int` | Lovelace value of the active UTxO. Decremented locally by the just-built tx's fee (read from the tx body) so we don't need a re-query. Reset on refund. |
| `refund_threshold` | `int` (lovelace) | Env `REFUND_THRESHOLD_LOVELACE`, default 5_000_000. Constant for the run. |
| `refund_topup` | `int` (lovelace) | Env `REFUND_TOPUP_LOVELACE`, default 100_000_000. Constant for the run. |
| `pacing_mode` | `"none" \| "await"` | Env `TX_GENERATOR_PACING`, default `"none"` (per [research.md R-005](./research.md)). |
| `await_timeout_seconds` | `int` | Env `TX_GENERATOR_AWAIT_TIMEOUT_SECONDS`, default 60. Used only when `pacing_mode = "await"`. |
| `tx_count` | `int` | Total submitted (self-pay + refund), exposed in logs. |
| `error_count` | `int` | Total caught + retried errors, exposed in logs. |

**State invariants**:
- `active_lovelace ≥ min_utxo + safety_margin` at every loop entry (asserted via SDK `always(active_utxo_above_min)` per plan Constitution Check II).
- `active_txin` is always the most recently produced self-pay or refund output; never re-fetched mid-chain.

**State transitions**:

```
[bootstrap]
  ↓ utxos_at(address)
  → pick UTxO with lovelace ≥ refund_threshold + safety
  → active_txin, active_lovelace

[loop: every iteration]
  if active_lovelace < refund_threshold:
    [refund]
      → faucet_txin = pick_faucet(utxos_at(address))      # exclude active_txin
      → build refund tx: ins=[active_txin, faucet_txin],
                         outs=[(address, refund_topup)],
                         change-address=address (drains rest to a faucet-shaped UTxO)
      → sign, submit, refund_txid
      → active_txin = (refund_txid, 0)
      → active_lovelace = refund_topup  (minus fee, computed from tx body)
  else:
    [self-pay]
      → build tx: ins=[active_txin], outs=[], change-address=address
                  → produces single output at (new_txid, 0)
      → sign, submit, new_txid
      → if pacing_mode == "await":
          await((new_txid, 0), timeout=await_timeout_seconds)
      → active_txin = (new_txid, 0)
      → active_lovelace -= fee  (read from tx body)
```

## E2 — `IndexerClient` (NDJSON over Unix stream socket)

Lives in `components/tx-generator/indexer_client.py`. Stateless — one short-lived connection per request. No connection pooling in v1 (the call rate is `< 1 Hz`).

### Public surface

```python
class IndexerUnavailable(RuntimeError): ...

class IndexerClient:
    def __init__(self, socket_path: str): ...

    def utxos_at(self, address: str) -> list[Utxo]:
        """Snapshot. Raises IndexerUnavailable on any failure."""

    def await_txin(self, txid: str, ix: int, timeout_seconds: int) -> AwaitResult:
        """Blocks until (txid, ix) is observed, or timeout. Raises IndexerUnavailable on connection failure."""

    def ready(self) -> ReadyStatus:
        """One-shot readiness check. Raises IndexerUnavailable on failure."""
```

### Wire conversation

| Method | Request line | Response line(s) |
|---|---|---|
| `utxos_at` | `{"utxos_at": "<bech32-or-base16-address>"}` | one line: `{"utxos": [{"txin": "<txid>#<ix>", "txout": "<base16-cbor>"}, ...]}`, then EOF |
| `await_txin` | `{"await": "<txid>#<ix>", "timeout_seconds": N}` | blocks; one line: `{"slot": <int>, "blockHash": "<hex>", "txout": "<base16-cbor>"}` or `{"timeout": true}`, then EOF |
| `ready` | `{"ready": null}` | one line: `{"ready": <bool>, "tipSlot": <int|null>, "processedSlot": <int|null>, "slotsBehind": <int|null>}`, then EOF |

Failure responses (errors from the daemon) take the shape `{"error": "<message>"}` and are raised as `IndexerUnavailable`. Connection-level failures (socket refused, EOF before a full line, non-JSON line) are also raised as `IndexerUnavailable`.

### Data shapes

```python
@dataclass(frozen=True)
class Utxo:
    txin: tuple[str, int]   # (txid, output index)
    txout_cbor_hex: str     # base16-encoded CBOR TxOut

@dataclass(frozen=True)
class AwaitResult:
    timed_out: bool
    slot: int | None
    block_hash: str | None
    txout_cbor_hex: str | None

@dataclass(frozen=True)
class ReadyStatus:
    ready: bool
    tip_slot: int | None
    processed_slot: int | None
    slots_behind: int | None
```

## E3 — Compose topology delta

The new entities introduced into `testnets/cardano_node_master/docker-compose.yaml`:

| Entity | Shape | Notes |
|---|---|---|
| `indexer` service | new container, image TBD per [`#78`](https://github.com/lambdasistemi/cardano-node-clients/issues/78), mounts `relay1-state:/state:ro` (relay socket source) and `indexer-sock:/sock` (own socket). Runs the daemon binary with `--relay-socket /state/node.socket --listen /sock/indexer.sock`. | Healthcheck per [research.md R-004](./research.md). |
| `indexer-sock` volume | new top-level docker volume; ephemeral. | Holds exactly one file: `indexer.sock`. |
| `tx-generator` service edits | add `indexer-sock:/sock:ro` mount; add `INDEXER_SOCKET_PATH=/sock/indexer.sock` env; add `depends_on: indexer: { condition: service_healthy }`. Bump image tag to a commit SHA from this branch. | Submission still uses `relay1-state:/state:ro` for `cardano-cli transaction submit` (FR-009). |
| `asteria-bootstrap`, `asteria-player-1`, `asteria-player-2` service edits | same `indexer-sock:/sock:ro` mount + same env + same `depends_on`. | Asteria implementation lives in PR [#67](https://github.com/cardano-foundation/cardano-node-antithesis/pull/67). |

See [contracts/compose-topology.md](./contracts/compose-topology.md) for the exact YAML diff.

## Cross-references

- The single-output / change-only tx shape that makes `(txid, 0)` deterministic: [research.md R-001](./research.md).
- Refund threshold + top-up defaults: [research.md R-002](./research.md).
- Failure model `IndexerUnavailable` raised once and caught once: [research.md R-003](./research.md).
- Pacing modes `"none"` vs `"await"`: [research.md R-005](./research.md).
- Daemon image-tag handling until [`#78`](https://github.com/lambdasistemi/cardano-node-clients/issues/78) publishes: [research.md R-006](./research.md).
