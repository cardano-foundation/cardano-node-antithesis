# Prompt — Cardano Run Health Report (project-first triage)

It produces an owner-oriented, project-first triage report of recent
Antithesis runs, rendered as a **public-safe markdown report** (a wiki page, **not** HTML),
separating **real client/SUT bugs**, **potential bugs**, and
**genuine Antithesis-platform faults**.

---

Generate a **project-first run-health report** for the last **7 days** of Antithesis runs on this tenant, **or** for an explicit **start–end date range / week** if one is given. Today's date is the reference point for the default window; skip anything outside the window. **`in_progress` vs `incomplete` are different statuses:** skip `in_progress` runs, but **do triage `incomplete` runs** via their `failure_reason` classes (§2) — they have finished and carry root-cause evidence.

**Week references:** if the window is given as a week (e.g. "week of the 20th"), resolve it to concrete dates and compute the **ISO week label** (`YYYY-Www`) the window falls in — the output page is named and filed by ISO week (see §4).

## 0. Preflight

- All data comes from the tenant's **v1 REST API** via `scripts/api_call.sh`. Auth: `ANTITHESIS_API_KEY` is sent as a bearer token — `curl -H "Authorization: Bearer $ANTITHESIS_API_KEY"`; `ANTITHESIS_PROXY=""` calls the API directly. `$BASE` below = `https://<tenant>.antithesis.com` (script default: `amaru-cardano`; override with `ANTITHESIS_BASE_URL`).
- Require `curl` + `jq`. Probe once before fanning out: `curl -sS -H "Authorization: Bearer $ANTITHESIS_API_KEY" "$BASE/api/v1/runs?limit=1"` must return JSON with `.data`.

## 1. Prefetch — run the script (one call, all the data)

```bash
bash scripts/api_call.sh --days 7 --out triage_prefetch.json
# or an explicit range:  bash scripts/api_call.sh --from <ISO> --to <ISO> --out triage_prefetch.json
```

The script pages through `/api/v1/runs` for the window, groups by **org / repo / directory** (this triple = one "project", **owner = the requester(s)**, parsed from the JSON-encoded run description with null-guards), and per project emits status counts plus:

- **every incomplete run pre-classified** from `failure_reason` (§2) — including the terminal build-logs `[STATUS]` line already captured as `evidence` where relevant;
- **failing properties prefetched for every completed run** (name, group, counterexample counts, up to 3 counterexample moments, one truncated sample); clean completed runs appear only as `clean_completed_ids`.
- **runs whose fetch failed after retries** appear as `fetch_failed_ids` (completed) or class `FETCH_FAILED` (incomplete). They carry no property data: list them in the project's detail as `⚠️ data unavailable (fetch failed)` with their run IDs, and count them as neither clean nor failing.

Everything downstream reads `triage_prefetch.json` — do **NOT** re-list runs or re-fetch per-run details already in it. `in_progress` runs appear as `in_progress_ids` (ignore them). The stdout table is the at-a-glance check that the window and grouping look right; the JSON's `total_runs` is the true total.

## 2. Triage each project (fan out — one agent per project)

Spawn parallel agents (general-purpose). Give each agent its project's slice of `triage_prefetch.json` and these rules. Agents make **on-demand API calls only for moment logs / event searches** — never re-fetch what the prefetch already holds, and **never open a triage report** (the WASM SPA doesn't render headless, and nothing below needs it — `failure_reason` already carries what the report's Error panel would say).

On-demand fetches (same auth as the script — `curl -sS -H "Authorization: Bearer $ANTITHESIS_API_KEY" …`):

- **Moment log:** `"$BASE/api/v1/runs/<run-id>/logs?input_hash=<h>&vtime=<v>"` (NDJSON; match vtime **exactly**, don't let it round).
- **Event search:** `-G "$BASE/api/v1/runs/<run-id>/events" --data-urlencode "q=<term>"` — find more pass/fail occurrences of a property.
- **More counterexamples:** `"$BASE/api/v1/runs/<run-id>/properties?status=Failing"` (paginate with `after=<next_cursor>`) — the prefetch keeps only the first 3 moments per property.
- **Full build-logs** (only for `BUILD_FAILED_UNPARSED` below): `"$BASE/api/v1/runs/<run-id>/build_logs"` (NDJSON, `.text`).

**Completed runs — failing properties** (prefetched under `completed_failing`):

- For each failing property, pull the moment log for at least one counterexample moment and analyze it; use the events search to establish the pass/fail timeline.
- Classify each failing property into one of §3's three buckets. Faults are expected, so a *transient* dip that recovers once the fault clears is a **potential bug** to confirm (see §3), not an automatic pass. A failure that **persists / never recovers**, or that reproduces with **zero faults active**, is a real SUT bug.

**Incomplete runs — already classified by the script from `failure_reason`**. The `class` field is mechanical; the agent's job is only what each class says below:

- **`ANTITHESIS`** — `failure_reason: "Antithesis session failed"`, or "Build failed" with a 3xxx `antithesis_error` in `evidence`. Platform failure, **final** — nothing to fetch, nothing to verify. (A `0/0` `failure_moment` alone is not the signal; the `failure_reason` / error code decides.) One-liner per §4 C.
- **`CLIENT_BUILD`** — reason "Build failed", terminal `[STATUS]` carries a **4xxx** `antithesis_error` (image unreachable / unauthorized / wrong registry path) → **CLIENT** (image/config). The `evidence` field holds the exact `[STATUS]` line — quote the image + message from it; no further fetches needed.
- **`BUILD_FAILED_UNPARSED`** — build failed but no `antithesis_error` code was extracted. Read the full build-logs yourself and classify from the terminal `[STATUS]`: 4xxx → CLIENT, 3xxx → ANTITHESIS.
- **`CLIENT_SETUP`** — reason "Container setup failure" / "No 'setup complete' event received". Client/SUT setup-phase, and it carries a **real `failure_moment`** → fetch that moment log to localize the crash (container exit, panic, missing image at runtime) and quote it as evidence. Lean CLIENT; reclassify only if the log unambiguously points at Antithesis's own binaries (signals below).
- **`OTHER`** — an unrecognized `failure_reason` string. Take the string at face value for the classification lean, then verify with the cheapest evidence: build-logs `[STATUS]` first, moment log if a moment exists. Still no report.
- **`UNKNOWN`** — `failure_reason` is null (these runs also carry **no `failure_moment` at all**). Root cause is **not retrievable from the API**. Classify as Antithesis with the callout **"session-phase failure"**; never default to client-side.

**Source-code correlation (only for projects whose source is checked out locally):** if the project's `repo` matches a checkout under `git/<repo>` in the current directory (e.g. `git/cardano-node-antithesis`, `git/ouroboros-leios`), trace each 🟥 Real / 🟨 Potential finding into that source: locate the failing property/assertion definition, the component that crashed or emitted the evidence line, or the setup script / compose config at fault. Draft a short **suggested fix** per finding — what to change, in which file (**repo-relative path**), and why it addresses the evidence. It feeds the project's "Suggested fix" dropdown in §4 C. Skip silently when there is no local checkout, and never draft fixes for 🟦 Antithesis findings.

**Platform-vs-client signals — match on the STRUCTURE, not on a specific message.** The error's `message`/`condition`/`location`/`exit_reason`/`backtrace` vary run to run — do NOT treat any one instance's text as the criteria. Classify on:

- `failure_reason: "Antithesis session failed"`, or an `antithesis_error` with a **3xxx** code → **Antithesis session/platform failure**, whatever the message says. (One observed 3xxx read `{"code":3001,"message":"Assertion","location":"./telemetry/event_synthesis/event_synthesizer.cpp:130","exit_reason":"hard_overskid"}` — an *example* of the shape; the next 3xxx will read differently.)
- The **`No Antithesis errors`** meta-property failing (see classification below).
- A `location`/`backtrace` pointing at **Antithesis's own binaries** (`fuzzer.debug`, `*.cpp` under the telemetry/event-synthesis/driver code) rather than the SUT → platform.
- Contrast — **client**, not platform: a **4xxx** `antithesis_error` (esp. `4001` image unreachable/unauthorized/registry-path), or a failure whose logs/backtrace point at the customer's containers, node, or setup scripts.
- **Capture, don't match:** once the class says platform, quote the actual `code` + `message` + `location`/`exit_reason` (and vtime) verbatim as the evidence for that run.

## 3. Classify every finding into exactly one bucket

- **Real (client/SUT)** — a bug/misconfig in the customer's node, images, workload, or setup scripts. Includes: spontaneous node crashes/panics, over-severe or wrong logging, setup/harness timeouts, missing/unauthorized images, wrong registry paths, cluster-generation script failures.
- **Potential bug** — a failing property that is not (yet) a confirmed SUT bug and is not an Antithesis issue: the evidence is inconclusive, or the property failed only *transiently* under faults and may have recovered. Faults being active does NOT make it "expected" — the SUT may still be required to hold, so flag it for the owner to confirm rather than dismiss it. Base the call on what the evidence actually shows (property status + pass/fail timeline + moment logs) — e.g. a `sometimes`/liveness property that never reached its target, or a safety property that dipped during a fault burst and then recovered. State what would confirm or clear it.
- **Antithesis-platform** — a fault in Antithesis itself, not the customer. Signals: `failure_reason: "Antithesis session failed"` or an **`antithesis_error` code 3xxx** (a session-phase platform failure, even when the build reached `ready:true`); the meta-property **`No Antithesis errors`** failing (its own description says it may reflect an Antithesis bug); internal errors not present in any exposed SUT/component channel; tooling edge cases (e.g. log-fetch rejecting a negative `input_hash`). Chase the triggering string via the moment-log / events API as far as you can — but **whatever you find or fail to find, the report gets only the one-liner from §4 C.** Don't narrate the retrieval attempt on the page.

Be conservative labeling something Antithesis-platform — only when it does NOT trace to customer code.

## 4. Output — one markdown report (wiki page), project-first, aimed at owners

Render a **single self-contained markdown file** — **not HTML**. It must be **public-safe** (this is the CF wiki's hard rule): **run IDs only, never authenticated/tokenized report URLs**, no secrets, no SSO cookies. GitHub renders `<details>`/`<summary>`, tables, and fenced code blocks natively, so use those for structure. Keep the layout **deterministic** — same sections, same order, every time.

**Filing & naming.** Write the file to `work/` and name it **by ISO week**: `Antithesis-Report-YYYY-Www.md` (e.g. `Antithesis-Report-2026-W30.md`). Start the page with a wiki header `# Antithesis platform report — YYYY-Www`, then an **Analyzed window** line (`YYYY-MM-DD → YYYY-MM-DD (UTC)` + run/project counts + tenant), then a link back to `[Antithesis-Reports](Antithesis-Reports)`. If the requested window doesn't align to a single ISO week, name by the ISO week the bulk of it falls in and state the exact analyzed dates in the window line.

**A. Header + window line** (as above). No separate stat band beyond the one window line.

**B. Projects at a glance** (top). One markdown table: project + directory, **owner (requester)**, runs, clean count, incomplete count, and a state (Healthy / Node bug / Setup issues / Fully blocked / **Antithesis failure**) — use a leading emoji as the "pill" (🟢/🟠/🔴/🟦). Below it, a one-line legend: 🟥 Real / 🟨 Potential Bug / 🟦 Antithesis.

**C. Per-project detail** — one **`<details>` block per project, collapsed by default** (no `open` attribute), **sorted by owner** (alphabetically; worst-first within an owner: fully blocked / antithesis failure → node bug → setup → healthy). The `<summary>` line reads exactly: `<b><owner-name></b> - project: <b><dirname></b> - repo: <b><org-name>/<repo></b>` + a one-line state — owner, dirname, and org/repo each wrapped in `<b>` tags. Inside:

- A one-line "what's happening" summary.
- **🟥 Real and 🟨 Potential findings as pointers:** bold title, short plain-language explanation, a fenced **evidence** code block (log line / assertion / moment, verbatim), a **"Pointer for `<owner>`:"** action line, and the **full run IDs** it applies to.
- **Suggested fix (only projects with a local source checkout — see §2 source-code correlation):** one additional **nested** `<details><summary>Suggested fix</summary>` block at the end of the project's detail, containing per-finding: the source-level cause (repo-relative file paths, symbol/property names) and the concrete suggested change. Public-safe (paths and code identifiers only, no local absolute paths). Omit the block entirely when there's no local checkout or nothing actionable — no placeholder.
- **🟦 Antithesis findings get a ONE-LINER: bold title + full run IDs, nothing else.** No explanation, no evidence block, no "Pointer for `<owner>`:" line, and no Antithesis-side commentary anywhere in the report. Shape: `**🟦 Antithesis — <property> failed in N runs.** \`<run-id>\`, \`<run-id>\``
- **Do NOT add a "No Antithesis callouts" note.** Silence = none.

**Before finishing**, run the public-safety check on the file: grep for `antithesis.com/report/…?`, session cookies/PASETO/Bearer, and `token=` params, and `grep -n '</invoke>\|</content>\|<antml' <report.md>` — stray tool-call syntax leaks into the end of a long written file and ships to a public wiki if unchecked. The file in `work/` is the deliverable; publishing is a separate step.

## Formatting rules

- **Always print run IDs in full** (e.g. `6ea304d97098b65fdfa8e0477dec05b7-56-17`), never truncated — they're needed for lookups.
- Attribute each project to its **owner = requester**, not to "client".
- Frame the whole report **by project**, not as a global client-vs-Antithesis split — but still tag each finding's classification and surface Antithesis callouts clearly per project.
- Ground every conclusion in log/property evidence; if a property has no moment/logs (404), say so and rely on its `details`.
- Note cross-cutting root causes (e.g. one SUT behavior causing multiple properties to fail) so the owner fixes once.
- Keep each project's "what's happening" summary to one or two sentences; merge findings that share a root cause.
