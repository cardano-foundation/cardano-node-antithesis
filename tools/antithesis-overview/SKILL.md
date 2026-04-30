---
name: antithesis-overview
description: >
  Parametric multi-run Antithesis triage entry point. Lists recent runs
  for a given tenant + GitHub repo, presents a digest as a Markdown
  GitHub gist, and hands off to antithesis-triage for per-report
  drill-down. Use when the user asks "what's going on in Antithesis",
  "summarize last N hours", or starts a triage session without a
  specific report URL in hand.
compatibility: Requires the playwright MCP (mcp__playwright__*) and the GitHub CLI (gh). Bundled antithesis-triage.js runtime version 3.0.0 (vendored from antithesishq/antithesis-skills).
---

# Antithesis Multi-Run Overview

Entry-point skill for Antithesis triage. Answers "what happened
recently?" before you commit to digging into a single report. Hands
off to `antithesis-triage` for per-report drill-down once you've
picked a target.

## Parameters

The skill takes a free-form argument string. Parse out:

- **`--tenant <name>`** — e.g. `cardano`. URL becomes
  `https://<tenant>.antithesis.com`. Default: `cardano`. Override
  with `$ANTITHESIS_TENANT` if set.
- **`--repo <owner/repo>`** — GitHub repo to filter on, e.g.
  `cardano-foundation/cardano-node-antithesis`. **Required** — ask
  the user if missing. Substring match on the row's
  `repository.organization/repo`.
- **`--hours-back <N>`** — default `48`.
- **`--requester <name>`** — optional substring filter on the
  webhook requester (e.g. `cfhal`, `saratomaz`).
- **`--public`** — publish digest as a public gist instead of secret.
  Default: secret. Public gists are search-indexed; do NOT use them
  for runs whose reports embed PASETO auth tokens (which is all of
  them, for at least 9 hours after generation).

If invoked with bare arguments like
`"last 48h, focus on cardano-node-antithesis"`, infer the repo from
the most-specific GitHub-style token in the string.

## When to use

- "What's going on in Antithesis for repo X?"
- "Any failures overnight?"
- "Summarize the last 24/48h"
- Morning triage where you don't yet know which run to look at
- Comparing the most recent runs of one repo across attempts

If the user hands you a single report URL and only wants that drilled
into, skip this skill and load `antithesis-triage` directly.

## Why playwright instead of agent-browser

The official `antithesis-triage` skill (upstream) assumes
`agent-browser`. On NixOS (and likely some other distros),
`agent-browser` fails to install because `libglib-2.0` isn't on the
linker path. This skill uses the playwright MCP (`mcp__playwright__*`)
instead — same browser, different driver. The vendored
`antithesis-triage.js` runtime is reused as-is.

## Setup: SSO cookie

Antithesis dashboards are gated by `__Host-antithesis_sso_session`.
Cookies last ~9 hours and are bound to the user's GitHub team
membership in the tenant's access team (e.g.
`pragma-org/antithesis-access`).

To prime the cookie when starting a session:

1. Ask the user to paste the cookie value (they get it from a
   logged-in browser via DevTools → Application → Cookies).
2. Inject it into the playwright context BEFORE navigating to a
   protected URL — the dashboard returns 403 for `/report/...` even
   with a valid cookie if you fail the navigation order.

```js
// playwright context cookie injection — run via mcp__playwright__browser_run_code
async (page) => {
  await page.context().addCookies([{
    name: '__Host-antithesis_sso_session',
    value: '<paste from user>',
    domain: '<tenant>.antithesis.com',
    path: '/',
    secure: true,
    httpOnly: true,
    sameSite: 'Lax',
    expires: <unix-seconds-from-cookie-Expires>,
  }]);
}
```

After setting the cookie, navigate to `/home` first to land in the
app context. Direct navigation to a `/report/...` URL with only the
SSO cookie returns 403 even when authenticated — you need a session
that the app has handed back to you. `/home` works; `/runs` works.

If the cookie is expired or rejected, ask the user for a fresh one.

## Workflow

This skill is interview-driven. Don't assume what the user wants —
ask, then act.

### Step 1 — open the runs page

```
mcp__playwright__browser_navigate https://<tenant>.antithesis.com/runs
mcp__playwright__browser_run_code:
  await page.addScriptTag({ path: '<this skill>/assets/antithesis-triage.js' });
  await page.evaluate(() => window.__antithesisTriage.runs.waitForReady());
```

`<this skill>` resolves to whatever Claude Code prints as
`Base directory for this skill: ...` when the skill is invoked.

### Step 2 — extract recent runs

Inject `assets/antithesis-overview.js` after the triage runtime is
loaded and call `getRecentRuns()`:

```js
mcp__playwright__browser_run_code:
  await page.addScriptTag({ path: '<this skill>/assets/antithesis-overview.js' });
  return await page.evaluate(() =>
    window.__antithesisOverview.getRecentRuns({
      hoursBack: <hoursBack>,
      requester: <requester or null>,
      repo: <repo>,
      onlyWithFindings: false,
    })
  );
```

Returns an array of:

```ts
{
  commit: string,            // 40-char SHA
  commitShort: string,       // first 8 chars
  commitUrl: string | null,  // https://github.com/<repo>/commit/<sha>
  try: number,
  testRunId: string,         // 64-char hex
  requester: string,
  repo: string,
  directory: string,
  startedLabel: string,      // "Today 9:52 AM" / "Yesterday 8:42 PM" / "Apr 28, 2026 …"
  status: string,            // "Completed" / "In progress" / "Failed" / "Stopped"
  duration: string,          // started→completed elapsed; see Pitfalls below
  findings: { new, ongoing, resolved, rare } | null,
  triageReportUrl: string | null,
  exploreLogsUrl: string,
}
```

`getRecentRuns` does not click anything; it parses the rows already
rendered. The page renders the most recent ~20 runs by default,
which covers ~2 days for active repos. If `hoursBack` exceeds what's
rendered, the function returns what it has and notes truncation.

### Step 3 — publish the digest as a GitHub gist

**Do NOT dump the digest into the terminal.** Antithesis report URLs
are 300–400 chars including the PASETO auth token and break
click-through in every terminal markdown renderer we've tried. Even
short URLs like GitHub commit links don't click reliably when
embedded in markdown table cells inside a terminal.

Instead, write the digest to a temp file as proper Markdown and
publish it via `gh gist create --secret`. GitHub renders the
markdown server-side, every link clicks correctly, and the terminal
stays clean. The user opens the gist URL once and reads the digest
in a browser.

Workflow:

1. Build the markdown locally — full table with proper Markdown
   links in every cell:
   - `Commit` cell: `[<commitShort>](<commitUrl>)`
   - `Triage` cell: `[report](<triageReportUrl>)` — or
     `[logs](<exploreLogsUrl>)` for in-progress runs
   - Issue references: `[#NNN](https://github.com/<repo>/issues/NNN)`
2. Write to `/tmp/antithesis-overview-<timestamp>.md`
3. `gh gist create --filename antithesis-overview.md --desc "Antithesis overview — <repo>, last <N>h, <date>" /tmp/antithesis-overview-...md`
   - Use `--secret` (the default) for runs containing private auth
     tokens. Public gists are search-indexed.
   - Caveat for the user: PASETO auth tokens have ~9h validity and
     a gist secret-URL is shareable.
4. Print **only** the gist URL to the terminal — nothing else.

Required table columns:

- `#` (1-based row index)
- `When (UTC)` (startedLabel)
- `Repo` (only when listing across multiple repos)
- `Directory` (only when it varies)
- `Commit` — Markdown link to GitHub commit
- `Try`
- `Requester` (drop when only one)
- `Status`
- `Elapsed` — labelled exactly that, with footnote ¹ when any row
  has `> 4h` (because elapsed includes queue time, see Pitfalls)
- `Findings (n/o/r/rr)` with non-zero `new` highlighted (`**N**` + 🚨)
- `Triage` — Markdown link to the report (or logs if in progress)

Below the table:

- **Suggested order of attack** — 2–4 candidates with rationale,
  each linking to its commit and report
- **Already triaged** — runs whose findings already have GitHub
  issues filed, link both

### Step 4 — drill into a selected run

The user opens the gist, picks a run, replies with `triage row N` or
similar. Hand that report URL off to `antithesis-triage` (or its
playwright-driven mirror) — its workflows expect you to be on
`/report/...` with the runtime loaded.

To navigate playwright there:

```
mcp__playwright__browser_navigate <triageReportUrl from row N>
mcp__playwright__browser_run_code:
  await page.addScriptTag({ path: '<this skill>/assets/antithesis-triage.js' });
  await page.evaluate(() => window.__antithesisTriage.report.waitForReady());
```

From there, the upstream `antithesis-triage` skill workflow applies:
`getPropertyExamples()`, `getExampleLogsUrl(name, idx)`, navigate to
the logs URL, `prepareDownload(0)`, etc.

### Step 5 — batch-download first failing example logs

For each failed property in a triaged report, download the first
failing example's `events.log` to
`/tmp/antithesis-<short>-<prop>.log`. Mechanics:

1. Use `report.getPropertyExamples()` to list failed properties
2. For each failed property:
   - Expand its container by `page.click` on
     `:scope > .property > .property__expander-button` — JS
     `.click()` does NOT trigger React's handler; you must dispatch
     via playwright
   - Pull the first example row's `a[href*='search']` href
3. Navigate to that Logs Explorer URL, inject the runtime,
   `await window.__antithesisTriage.logs.waitForReady()`,
   `prepareDownload(0)`, then click
   `a.sequence_printer_menu_button[data-triage-dl]` while listening
   for the `download` event
4. `download.saveAs(/tmp/...)`

Use one short tag per property in the filename so multiple downloads
don't overwrite each other.

### Step 6 — close out

Always summarize what was downloaded and where, even if the user
didn't ask for a final summary. Pattern:

```
Downloaded:
- /tmp/antithesis-<run1>-<prop1>.log (NN.M MB)
- /tmp/antithesis-<run2>-<prop2>.log (NN.M MB)

Open questions:
- ...
```

## Pitfalls / lessons learned

- **Terminals don't click long Markdown links inside tables.** Even
  short ones (~80 chars like GitHub commit URLs) wrap unpredictably
  inside a table cell and become un-clickable. The Antithesis
  PASETO-bearing report URLs (~400 chars) are completely hopeless.
  Solution: never render the digest to the terminal — publish it to
  a `gh gist --secret` and print only the gist URL. This is
  step 3 of the workflow, not optional.
- **Elapsed vs wall_clock — they don't match.** The runs index
  page's "duration" column is **started→completed elapsed time**,
  which includes queue + execution + post-processing + triage. The
  actual run wall-clock is in the report metadata (`wall_clock`
  field on `getRunMetadata()`). Real example: a run that the runs
  page showed as `9h 4m` had `wall_clock: 3h 7m` in its report.
  **Never call the runs-page column "duration" or "wall_clock" in
  user-facing output — use "elapsed" and footnote the distinction.**
- **Runs row uses a custom `<a-row>` element**, not a regular
  `<div>` or `<tr>`. The runtime's `rowsContainerCandidates()`
  selects `a-row` first; if Antithesis ever changes this, fall back
  to the div/li/tr scan that requires a single `testRunId` blob and
  the action-text presence.
- **`Triage results` and `Explore Logs` are anchors with hrefs in
  the DOM** — the URLs include the auth token. Do not click and
  capture via popup; just read `a.href` directly. (Disabled "Triage
  results" for in-progress runs renders as `<a-button>` instead of
  `<a>` and has no href.)
- **403 on `/report/...`** even with valid SSO cookie: navigate to
  `/home` first. The app-handed session token is only granted after
  a page-level handshake.
- **`.property` elements have empty `.property__details` until
  expanded** — example rows are populated lazily on click. Don't
  conclude "no rows" from a collapsed container.
- **`element.click()` from the page eval does NOT trigger React
  handlers** — use the playwright `page.click(selector)` or
  `mcp__playwright__browser_click`.

## Reference: existing related skills

- `antithesis-triage` (upstream
  [antithesishq/antithesis-skills](https://github.com/antithesishq/antithesis-skills))
  — single-report drill-down. Hand off after Step 4.
- `tools/query-logs/` (this repo) — Logs Explorer queries (cascade
  detection, count failures). Use to validate hypotheses raised
  during overview triage.

## Future work

A scriptable CLI replacement is tracked in
[#97](https://github.com/cardano-foundation/cardano-node-antithesis/issues/97).
When that lands, this skill should defer to it for runs-listing and
only use the browser for report-level drill-downs. Until then,
browser-driven is the only path.
