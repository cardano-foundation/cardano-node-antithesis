# antithesis-query-logs

> **Status: workaround, not a supported API.**
> Antithesis does not currently publish a programmatic interface for
> their Logs Explorer. This repo is a stopgap — a reverse-engineered
> client we use to unblock CI and scripted triage until Antithesis ship
> an official API. Everything here is observable from a logged-in
> browser session; nothing is privileged. Expect to delete this repo
> the day a real API lands.

Query [Antithesis Logs Explorer](https://antithesis.com/) for a specific
test run **without a GUI browser**. Reverse-engineered recipe for
pulling matches out of `/search` over CDP.

Works in two modes:

- **Headed** — a real chromium window where you complete Google SSO
  yourself; the tooling pulls the cookie straight out over CDP.
- **Headless** — paste a single `Copy as cURL` blob from your laptop's
  DevTools; the tooling greps the cookie out. No DOM-hunting.

## Prerequisites

- `bb` (Babashka) — <https://babashka.org>
- `curl`, `base64`, `chromium` (nixpkgs is fine)
- `agent-browser` on `$PATH` (≥ v0.23.4): `npm install -g agent-browser`
- A live Antithesis SSO session — you complete the Google login once
  per ~8h, either through `login.bb` (headed) or by pasting the curl
  blob (headless)

Set `ANTITHESIS_TENANT` if the tenant is not `cardano` (the default).

## Quickstart

```bash
# One-time cookie intake (pick A or B below), then:
scripts/query.bb --latest '"sev":"Warning"' --source log-tailer
```

Produces a structured summary: match count, breakdown by `source`, by
`(source, ns, sev)`, and the first 10 distinct rendered rows.

### Selectors (pick one, default `--latest`)

- `--latest` — most recent `Completed` run with a real `session_id`
- `--commit <sha>` — run whose params mention `<sha>`
- `--session <id>` — pass the Logs Explorer `s` directly

### Query options

- `--op contains|equals|regex` — matcher on `general.output_text`
- `--source <name>` — add a `general.source = <name>` AND clause
- `--count-only` — emit just the integer count (CI-friendly)
- `--raw` — emit the raw browser body dump, skip parsing
- `--wait-seconds N` — page-load budget (default 30)
- `--from-dump <file>` — parse a previously-captured body file instead
  of launching chromium (useful for debugging the parser)

## Cookie intake

### A. Interactive browser login (requires a display)

```bash
scripts/login.bb
```

Opens a real chromium window on the persistent profile
(`~/.cache/agent-browser-triage/profile`), navigates to
`https://<tenant>.antithesis.com/`, and waits for you on stderr:

```
A chromium window is open. Complete the Google login flow.
When you see the tenant dashboard, come back here and press Enter.
>
```

Complete the Google SSO flow normally, return to the terminal, press
Enter. `login.bb` reads `__Host-antithesis_sso_session` out of chromium
over CDP, writes it to `$ANTITHESIS_COOKIE_FILE` (default
`/tmp/antithesis-cookie.txt`) at mode 0600, and prints nickname +
expiry + a `root=302 follow=200` sanity check. The persistent profile
keeps Google session state between runs, so subsequent logins are
often a single click.

`login.bb` fails fast with a helpful message if `$DISPLAY` and
`$WAYLAND_DISPLAY` are both unset. Use path B on headless machines.

### B. Paste from DevTools (headless machines)

```bash
scripts/set-cookie.sh
```

Accepts either shape:

- the bare PASETO value (`v2.public.…`), or
- the entire `Copy as cURL` blob from DevTools' Network tab — the
  script greps `__Host-antithesis_sso_session=v2.public.…` out itself.

The fast sequence: in your laptop's already-logged-in tab on
`<tenant>.antithesis.com`, open DevTools → Network → right-click **any**
request → **Copy → Copy as cURL**. SSH to the headless machine, run
the command above, paste the entire curl blob, Enter.

The read prompt is silent (`read -rs`) so the value does **not** land
in shell history, terminal scrollback, or any transcript the session
may be capturing.

## Find a run (when not using `--latest`)

```bash
scripts/pangolin-runs.bb --session-ids            # table: time + session_id
scripts/pangolin-runs.bb --latest-completed       # single session_id
scripts/pangolin-runs.bb --by-commit <sha>        # JSONL, filtered
scripts/pangolin-runs.bb                          # raw JSONL of all runs
```

### Why the three magic headers are mandatory

`/api/pangolin/v1/WYATT/kv_table/runs` returns **403** with just the
SSO cookie. It only flips to 200 SSE when the request includes **all
three**:

- `X-Antithesis-Allow-Effects: 1`
- A browser `User-Agent`
- `content-type: application/json`

Missing any one = empty-body 403 that looks like an auth failure.
`pangolin-runs.bb` sets all three.

### Why session_ids are filtered

Webhook aggregator runs (e.g. `tenant_data_aggregator.nb2`) show up
with a synthetic session_id like `"OTIS"` that does not open in Logs
Explorer. The resolver only accepts ids matching
`^[0-9a-f]{32}-\d+-\d+$`.

## Build & open the search directly

`query.bb` wraps both, but the primitives are independently useful:

```bash
scripts/build-search-url.bb <session_id> <needle> [--op OP] [--source NAME]
scripts/open-search.bb      <session_id> <needle> [--op OP] [--source NAME] [--wait-seconds N]
```

`open-search.bb` launches chromium (headless) on a random CDP port,
injects the cookie at the tenant origin, navigates to the encoded URL,
clicks the primary **Search** button
(`<a-button variant="primary" icon="search">` — **not** the play-icon
`.action_run_button_wrapper`, which stays disabled until validation),
and polls the DOM for the results pane to render and for
`Initializing query…` / `Loading results` to clear before dumping.

The full body is always written to `/tmp/open-search-last.txt` so a
failed run is inspectable without rerunning.

On failure, see [`references/driver-troubleshooting.md`](references/driver-troubleshooting.md).

## What Antithesis actually indexes

Load [`references/indexed-sources.md`](references/indexed-sources.md)
before querying for a string that cannot possibly appear. Short
version: **container stdout only**. If a log stream lives inside a
docker volume it is not captured. For per-node tracer events indexed
by Antithesis, emit them via a sidecar that tails the file out to its
own stdout (e.g. a `log-tailer` container).

## Common failure modes

| Symptom | Likely cause |
|---|---|
| `root=302 follow=302` from set-cookie | Cookie expired or wrong tenant |
| `403` from pangolin-runs | Missing one of the three mandatory headers |
| Resolver returns `OTIS` | session-id-pattern filter bypassed — bug |
| Search page loads but no results | Button click not firing — check selector, check `/tmp/open-search-last.txt` |
| `session_id` mismatch | Using the GUI's `testRunId`; use the pangolin resolver's id |
| Chromium exits immediately (exit 144) | Sandboxed host; re-run from an unrestricted shell |
| `login.bb`: `error: no display available` | You're on a headless host — switch to path B |

## Self-review before reporting results

- Did the pangolin fetch return the run you expected? Cross-check
  commit, try, and `antithesis.images` against what you submitted.
- Is the `session_id` present (non-empty) for that run? Runs that
  failed to start (image-pull timeout) have no `session_id`.
- Did the search page actually render results, or is it stuck on the
  "Explore your logs" empty state? Empty state = zero matches (a
  valid negative), not an error.

## License

Apache 2.0 — inherits the repo's top-level [LICENSE](../../LICENSE).
