# Logs Explorer /search URL format

The Logs Explorer encodes its entire query state into a single URL
parameter so that any search can be reproduced just by sharing the link.

## Overall URL

```
https://<tenant>.antithesis.com/search?search=v5v<base64url(json)>&report_name=&get_logs=false&get_logs_event_desc=
```

- `v5v` is a literal schema marker — **not** part of the base64.
- The base64 is url-safe (`+`→`-`, `/`→`_`) with padding stripped.
- `report_name`, `get_logs`, `get_logs_event_desc` are always present
  but typically empty for ad-hoc queries.

## JSON shape

```json
{
  "q": {
    "n": {
      "r": {
        "h": [
          {
            "h": [
              {
                "c": false,
                "f": "general.output_text",
                "o": "contains",
                "v": "<needle>"
              }
            ],
            "o": "or"
          }
        ],
        "o": "and"
      },
      "t": {"g": false, "m": ""},
      "y": "none"
    }
  },
  "s": "<session_id>"
}
```

### Field by field

| Path | Meaning |
|---|---|
| `q.n` | The query tree |
| `q.n.r` | Root clause — an AND of OR groups |
| `q.n.r.h[]` | AND-joined groups |
| `q.n.r.h[].h[]` | Leaves within one OR group |
| `q.n.r.h[].h[].c` | Case-sensitive flag (bool) |
| `q.n.r.h[].h[].f` | Field name, e.g. `general.output_text` |
| `q.n.r.h[].h[].o` | Operator: `contains` / `equals` / `regex` |
| `q.n.r.h[].h[].v` | Value |
| `q.n.r.h[].o` | How leaves combine: `or` / `and` |
| `q.n.r.o` | How groups combine: `and` / `or` |
| `q.n.t` | Time filter (`g` = has-range bool, `m` = mode) |
| `q.n.y` | Correlation type (`none`, `event`, ...) |
| `s` | Session id `<hex32>-<major>-<minor>` (the run) |

### Known fields to filter on

- `general.output_text` — stdout content of any container
- `general.source` — emitter name (e.g. `p1`, `log-tailer`)
- `general.container` — docker container name
- `general.virtual_time` — simulated wall-clock

You can usually get away with adding a clause to filter by
`general.source = log-tailer` to isolate one sidecar's output.

## Why the indirection

The app is a SPA with a Monaco-based query builder. Every edit of the
builder re-serialises to this JSON and pushes a new URL. The backend
renders the same query from the URL on first load, so URLs are
reproducible and shareable.

If the `s` is stale/invalid the page loads but returns zero results
silently — there is no 4xx response. Always cross-check against
pangolin's `kv_table/runs` that the session_id exists.
