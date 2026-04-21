# What Antithesis indexes and what it does not

The triage report's Logs Explorer covers **container stdout only** for
cardano-foundation tenant testnets. Anything that goes to a file inside
a container, or to a shared docker volume, is invisible — even if it is
later read by another container.

| Source | Goes where | Captured? |
|---|---|---|
| `cardano-node` stdout | docker logs (~16-18 lines: startup banner + one-off `Reflection.TracerInfo` / `Reflection.TracerConfigInfo` JSON) | **Yes** — indexed as `general.output_text`, `source=pN` |
| `cardano-node` tracer stream | forwarded over the tracer socket to `cardano-tracer`, which writes ForMachine JSON to `/opt/cardano-tracer/logs/<host>_3001/node-*.json` on the shared `tracer` docker volume | **No** — volume files are not archived |
| `cardano-tracer` stdout | docker logs | Yes |
| sidecars (`tracer-sidecar`, `sidecar`, `configurator`) | docker logs | Yes — this is where Antithesis SDK assertions are emitted |
| `log-tailer` sidecar | docker logs (after workaround) | Yes — see below |

## Empirical confirmation

Run `s=4a229d0363682145d1831acdf28952a7-50-7`:

| Needle | Match count | Interpretation |
|---|---:|---|
| `TracerMeta` | 7,415 | Indexed — startup reflection JSON |
| `AddedBlockToVolatileDB` | 7,467 | Indexed — but only inside the startup `TracerConfigInfo` JSON that happens to list namespace names; no actual block events |
| `TraceAddBlockEvent` | 0 | **Not** indexed — steady-state block trace |
| `ForgedBlock` | 0 | **Not** indexed — forge event |
| `ConnectionHandler` | 0 | **Not** indexed — mux/connection trace |

So the rich per-node trace stream (1-2 MB per host after 30 minutes,
locally verified) exists on disk in the `tracer` volume but never
reaches triage.

## The log-tailer workaround

Added in `cardano-foundation/antithesis-tx-gen-and-ogmios` PR #44. A
tiny Alpine sidecar mounts the `tracer` volume read-only, polls the
log directory, and `tail -F`s every `node-*.json` file to its own
stdout — filtered to `sev ∈ {Warning, Error, Critical}` so we do not
bury the stream in routine noise.

Each emitted line becomes an indexed `general.output_text` event under
`source=log-tailer`. To verify it is working on a specific run:

```bash
./scripts/open-search.sh <session_id> '"sev":"Warning"'
```

Zero matches during steady-state operation is a valid result — the
testnet may simply not have hit any Warnings. To prove the sidecar is
connected regardless, search for something it always emits at startup
(e.g. a tracer namespace that appears on boot).

## Do not trust apparent matches on config-echo JSON

`AddedBlockToVolatileDB` returns thousands of hits but every one is
inside the same startup `TracerConfigInfo` event that lists every
tracer namespace name. It is not evidence that block events are
indexed. Any string that appears in the namespace catalogue will
match this way. When checking if a specific trace type is captured,
search for a value that would only appear at runtime (e.g. a slot
number, a hash, a severity field) rather than a namespace label.
