# Contract: Indexer Client (NDJSON over Unix socket)

**Feature**: 003-indexer-read-side
**Spec**: [../spec.md](../spec.md) | **Plan**: [../plan.md](../plan.md) | **Data Model**: [../data-model.md](../data-model.md)
**Daemon**: [`lambdasistemi/cardano-node-clients#78`](https://github.com/lambdasistemi/cardano-node-clients/issues/78) (the producer of this contract)

This is the wire contract consumed by `tx-generator` (Python) and `asteria-player` (Haskell, in PR [#67](https://github.com/cardano-foundation/cardano-node-antithesis/pull/67)). The daemon is the producer; this document is the consumer-side specification of the same wire.

## Transport

- Unix domain socket, `SOCK_STREAM`, default path `/sock/indexer.sock` (configurable per-consumer via env `INDEXER_SOCKET_PATH`).
- One subscription per connection. Close the socket to cancel. No multiplexing.
- Wire format: newline-delimited JSON (NDJSON), UTF-8, one JSON object per line. `\n` is the only line terminator.

## Requests

### REQ-1: `utxos_at` — snapshot at address

**Request line**:
```json
{"utxos_at": "<bech32-or-base16-address>"}
```

**Response (one line, then EOF)**:
```json
{"utxos": [{"txin": "<txid>#<ix>", "txout": "<base16-cbor>"}, ...]}
```

- `txid` is hex (64 chars, lowercase). `ix` is a non-negative integer.
- `txout` is base16-encoded CBOR of the Cardano `TxOut` (era-appropriate). Consumers either deserialize via cardano-cli helpers (Python: shell out is acceptable; Haskell: `cardano-ledger-*` decoder) or, for the address-and-value-only use case in this spec, parse only the lovelace and asset entries from the CBOR.
- An empty `utxos: []` response means "no UTxOs at this address right now"; this is not an error.

**Error responses**:
- `{"error": "<message>"}` — daemon failed to serve the request (e.g. address parse failure). Raised as `IndexerUnavailable`.

### REQ-2: `await` — block until `(txid, ix)` appears

**Request line**:
```json
{"await": "<txid>#<ix>", "timeout_seconds": <int>}
```

- `timeout_seconds` is optional; if omitted, the daemon waits indefinitely (until consumer closes the socket).

**Response (one line, then EOF)**:
- On observation: `{"slot": <int>, "blockHash": "<hex>", "txout": "<base16-cbor>"}`
- On timeout: `{"timeout": true}`

**Cancellation**: consumer closes the socket. Daemon discards the subscription.

**Rollback semantics** (per [spec.md](../spec.md) wait primitive — final): the daemon emits the response line on the first observation. If the observed block is rolled back before the response line is written, the daemon stays open and waits for the next observation. Once the line is written, the daemon closes the socket; rollback handling after that point is the caller's concern (rediscoverable via `utxos_at`).

### REQ-3: `ready` — readiness probe

**Request line**:
```json
{"ready": null}
```

**Response (one line, then EOF)**:
```json
{"ready": <bool>, "tipSlot": <int|null>, "processedSlot": <int|null>, "slotsBehind": <int|null>}
```

- `ready=true` ⇔ `slotsBehind ≤ K` for a daemon-internal threshold K (consumer doesn't need to know K).
- All slot fields may be `null` during the very first moments after the daemon starts and before it has observed any chain-sync activity.

This request is also what the docker compose healthcheck uses (per [../research.md R-004](../research.md)).

## Failure model

- **Connection refused / EOF mid-line / unparseable JSON / `{"error": ...}` response**: consumer raises `IndexerUnavailable` (or language-equivalent). One catch site per consumer, in the main loop. Surfacing the error and pausing the workload is the only legal response. Falling back to `cardano-cli query utxo --address` is **forbidden** (FR-006).
- **Slow daemon** (long catch-up, `slotsBehind > 0`): consumers see `ready=false` from the healthcheck and don't start until the daemon goes ready. After startup, momentary lag is tolerated — `utxos_at` returns the daemon's last-applied set; consumers proceed with the deterministic-chain assumption.
- **Daemon crash mid-await**: consumer sees EOF on the read; raise `IndexerUnavailable`.

## Antithesis SDK assertions on this contract

Consumers MUST emit at least the following SDK signals during steady-state operation (constitution Principle II):

| Trigger | SDK call | Meaning |
|---|---|---|
| First successful `ready=true` after consumer startup | `reachable("indexer_ready")` | Indexer is wired in and reachable. |
| Refund tx is built | `sometimes("refund_triggered")` | Both this branch and the no-refund branch must be observed under fault injection. |
| About to build self-pay or refund | `always("active_utxo_above_min", <bool>)` | Invariant — must be true before every build. |
| Caught a non-`IndexerUnavailable` exception that masked an indexer failure | `unreachable("indexer_silently_unreachable")` | Defensive — the SDK report flags it if the failure model is ever bypassed. |

## Out-of-scope (not in v1)

- Policy/asset-keyed lookups (`utxos_by_policy`). Consumers filter `value` in-process for asset-class discovery.
- Cancel-message vocabulary (`{"cancel": ...}`). One subscription per connection; close to cancel.
- Streaming subscriptions returning more than one response line. Every v1 request emits exactly one response line then EOF.

## Versioning

This contract is v1. Future additions (e.g. policy-keyed snapshot, multi-line streams) are additive and gated behind a new request kind, not a wire-level versioning header.
