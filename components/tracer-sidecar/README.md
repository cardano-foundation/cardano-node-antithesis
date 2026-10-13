# tracer-sidecar

`tracer-sidecar` reads and processes cardano-node logs from disk, line by line, and is able to
write antithesis [assertions](https://antithesis.com/docs/properties_assertions/) to `$ANTITHESIS_OUTPUT_DIR/sdk.jsonl`.

The idea is to use it to test properties that a Cardano network should satisfy, as well as
using sometimes-assertions to guide the fuzzer into finding more interesting scenarios more often.

## Amaru stdout ingestion

When the `AMARU_LOG_DIR` environment variable is set, the sidecar also watches
that directory for two fixed relay log files (`amaru-relay-1.log` and
`amaru-relay-2.log`) and ingests each plain line into the scored event
pipeline. Files created after the watcher starts are discovered automatically.

### Configuration

| Variable | Required | Effect |
|---|---|---|
| `AMARU_LOG_DIR` | No | Enables Amaru ingestion when set to a directory path. When absent, none of the Amaru-specific properties is declared. |
| `SECURITY_PARAM` | No | Praos `k` of the testnet (its `securityParam` in `testnet.yaml`). Defaults to 432, the master and adversary testnets. |

### Normalized event shape

Each ingested line becomes a `LogMessage` with:

- **source**: `amaru-container-stdout`
- **host**: relay identity derived from the filename (e.g. `amaru-relay-1`)
- **message**: the complete original line, newline stripped
- **severity**: `Info` (cannot trip the existing `no critical logs` rule)

### Scored properties

When Amaru ingestion is enabled, three properties are declared:

- **`amaru stdout observed`** (Sometimes, required) — hit on the first
  normalized Amaru stdout event. Proves the route is alive; a disconnected
  route cannot appear green.
- **`no fatal amaru consensus logs`** (AlwaysOrUnreachable) — fails once when
  any exact case-sensitive signature is observed:
  - `Consensus died`
  - `lifecycle.consensus_died` (the line current Amaru logs when a consensus
    stage exits)
  - `attempted roll back in the future`

  The failure payload carries the source, relay identity, and complete
  original line as evidence. Ordinary output does not fail this invariant.

### Non-vacuity

The required `amaru stdout observed` property prevents the fatal-consensus
invariant from passing vacuously: if no Amaru line ever reaches the scored
stream, the run reports a missing required property rather than a green board.

### Following the producers

- **`amaru relays follow the producer tip`** (AlwaysOrUnreachable) — fails once
  when an Amaru relay has stayed more than `k` blocks behind the longest tip of
  the producers `p1..pN` for 300 seconds. The relay's height is the latest
  `block_height` of its `tip.adopt` lines; the failure payload names the relay,
  both heights, when the lag began and `k`. A relay that is restarted by fault
  injection starts from its stored tip and catches up within seconds, which the
  window tolerates; a relay that rejects a block and stays at its tip does not.
  A relay that never logs a `tip.adopt` line is not judged by this property.

## Cluster fork depth

`cluster fork depth < k` fails once a host would have to roll back `k` blocks or
more to follow the longest tip. A host whose tip lies on the longest chain is
behind, not forked, and needs no rollback, so a node that is catching up (such
as a consumer syncing from Amaru) does not count. A host whose ancestry cannot
be placed against the longest chain within the walk cap is not judged.
