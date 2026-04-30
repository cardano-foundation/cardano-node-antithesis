# antithesis-overview

> **Status: workaround, not a supported API.**
> Antithesis does not currently publish a programmatic interface for
> their runs index or report pages. This tool is a stopgap — a
> reverse-engineered skill we use to make daily triage tractable. It
> drives a real chromium over CDP via the playwright MCP, scrapes
> rendered DOM, and never touches anything privileged. The day
> Antithesis ship an API this directory should be deleted.

A Claude Code skill that lists recent Antithesis runs for a given
GitHub repo, presents a digest as a GitHub gist (with clickable
report/commit links), and hands off to upstream `antithesis-triage`
for per-report drill-down.

## When to use this vs. the other tools

| Question | Tool |
|---|---|
| "What's going on in Antithesis for repo X over the last 48h?" | **`antithesis-overview`** (this dir) |
| "Why did this single assertion fail?" | [`antithesis-triage`](https://github.com/antithesishq/antithesis-skills) — parses one report HTML |
| "How many `sev:Warning` events came from source=p1 in run X?" | [`tools/query-logs/`](../query-logs/) |

This tool's job is the **morning-triage entry point**: turn the
cardano.antithesis.com runs page into a clickable Markdown digest
with a row per recent run, finding counts, and links to commits and
report URLs. From there you pick which run to drill into.

## Prerequisites

- **Claude Code** — this is an [agent skill](https://www.anthropic.com/news/skills),
  invoked via the Skill tool
- **playwright MCP** configured in your Claude Code MCP servers (the
  package is `@playwright/mcp` on npm). If you can't use playwright,
  the upstream `antithesis-triage` skill works with `agent-browser`
  instead — but `agent-browser` fails on NixOS, which is why this
  skill exists
- **`gh`** (GitHub CLI), authenticated — the digest is published as a
  secret gist
- **An Antithesis SSO session** — you complete the GitHub login once
  per ~9h and paste the `__Host-antithesis_sso_session` cookie when
  the skill asks for it

## Install (project-local)

The skill lives in this repo. To make it discoverable by Claude Code
without copying it into your home directory, symlink it into your
local `~/.claude/skills/` folder:

```bash
# from the repo root
mkdir -p ~/.claude/skills
ln -s "$(pwd)/tools/antithesis-overview" ~/.claude/skills/antithesis-overview
```

Restart Claude Code (or run `/refresh-skills` if your build supports
it) and the `antithesis-overview` skill should appear in the skill
list.

## Install (one-off, no symlink)

If you just want to try it once, point the skill's path explicitly
when you ask Claude Code to run it. The SKILL.md uses
`<this skill>/assets/...` placeholders that resolve to whatever
Claude Code prints as `Base directory for this skill: ...`.

## Usage

```
/antithesis-overview --repo cardano-foundation/cardano-node-antithesis --hours-back 48
```

Free-form arguments work too:

```
/antithesis-overview last 48h, focus on cardano-node-antithesis
```

### Parameters

| flag | default | description |
|---|---|---|
| `--tenant <name>` | `cardano` (or `$ANTITHESIS_TENANT`) | URL becomes `https://<tenant>.antithesis.com` |
| `--repo <owner/repo>` | **required** | GitHub repo to filter on, substring match |
| `--hours-back <N>` | `48` | Cutoff for "recent" |
| `--requester <name>` | none | Filter by webhook requester (e.g. `cfhal`) |
| `--public` | secret | Make the gist public — search-indexed, NOT recommended for runs whose reports embed PASETO auth tokens |

### What you get back

A single GitHub gist URL printed to the terminal. Open it in a
browser. The gist contains a Markdown table with one row per recent
run:

- **Commit** — clickable link to the GitHub commit
- **Triage** — clickable link to the Antithesis report (PASETO auth
  token embedded; valid for ~9h)
- **Findings** — `new/ongoing/resolved/rare`, with `new` highlighted
- **Suggested order of attack** — 2–4 candidates worth digging into

### Caveats

- **PASETO tokens leak in gist content.** A secret gist is
  unlisted, but anyone who learns the gist URL can read every
  embedded report URL for the duration of the token's ~9h lifetime.
  Don't post gist URLs to public channels.
- **The "Elapsed" column is misleading.** The runs page's "duration"
  is started→completed elapsed time (queue + execution +
  post-processing). Real run wall-clock is in
  `getRunMetadata().wall_clock` inside the report. The skill labels
  the column "Elapsed" and footnotes the distinction.
- **Cookies expire after ~9 hours.** Get a fresh one from a
  logged-in browser when the skill returns 403.

## Files

- [`SKILL.md`](SKILL.md) — Claude Code skill definition and workflow
- [`assets/antithesis-overview.js`](assets/antithesis-overview.js) —
  Logs index page parser (DOM scraping over `<a-row>` elements)
- [`assets/antithesis-triage.js`](assets/antithesis-triage.js) —
  Vendored from
  [antithesishq/antithesis-skills](https://github.com/antithesishq/antithesis-skills)
  v3.0.0. Used by the per-report drill-down flow. Update by
  re-copying the upstream file and bumping the version note in the
  SKILL.md frontmatter.

## See also

- [`docs/overview.md`](../../docs/overview.md) — user-facing guide,
  rendered on the mkdocs site
- [`docs/triage.md`](../../docs/triage.md) — single-report
  drill-down flow
- [`docs/query-logs.md`](../../docs/query-logs.md) — Logs Explorer
  search

## Future work

Tracked in
[#97](https://github.com/cardano-foundation/cardano-node-antithesis/issues/97):
a real scriptable CLI tool with sqlite caching, daily digest
generation, and no browser dependency. Once that lands, this skill
should be deleted in favour of calling the CLI from a thin Claude
wrapper.
