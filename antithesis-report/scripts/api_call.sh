#!/usr/bin/env bash
#
# Antithesis run-health prefetch
#
# Fetches every run in a time window from the v1 API, classifies incomplete
# runs via failure_reason (no report opening), prefetches failing properties
# for completed runs, and emits ONE compact JSON file grouped by project
# (org/repo/dir) plus a human summary table on stdout.
#
# Usage:
#   ./api_call.sh [--days N] [--from ISO --to ISO] [--out FILE] [--jobs N]
#
#   --days N     Window = last N days (default: 7). Ignored if --from given.
#   --from/--to  Explicit ISO8601 bounds (--to defaults to now).
#   --out FILE   Output JSON (default: triage_prefetch.json).
#   --jobs N     Parallel per-run fetches (default: 8).
#
# Requires: curl, jq. Auth: uses sandbox proxy (localhost:3000) with the literal bearer
# token "proxied" (real credentials are injected by the proxy).
set -euo pipefail

BASE="${ANTITHESIS_BASE_URL:-https://amaru-cardano.antithesis.com}"
DAYS=7 FROM="" TO="" OUT="triage_prefetch.json" JOBS=8

while [[ $# -gt 0 ]]; do
  case "$1" in
    --days) DAYS="$2"; shift 2 ;;
    --from) FROM="$2"; shift 2 ;;
    --to)   TO="$2"; shift 2 ;;
    --out)  OUT="$2"; shift 2 ;;
    --jobs) JOBS="$2"; shift 2 ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done

[[ -z "$FROM" ]] && FROM="$(date -u -d "-${DAYS} days" +%Y-%m-%dT%H:%M:%SZ)"
[[ -z "$TO"   ]] && TO="$(date -u +%Y-%m-%dT%H:%M:%SZ)"

fetch() { curl -sS -m 60 -x "${ANTITHESIS_PROXY:-http://localhost:3000}" \
                 -H "Authorization: Bearer ${ANTITHESIS_API_KEY:-proxied}" "$@"; }

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
mkdir -p "$TMP/runs"

# ---------------------------------------------------------------- 1. list runs
echo "Listing runs $FROM -> $TO ..." >&2
cursor=""
while :; do
  page="$(fetch -G "$BASE/api/v1/runs" \
            --data-urlencode "limit=100" \
            --data-urlencode "created_after=$FROM" \
            --data-urlencode "created_before=$TO" \
            ${cursor:+--data-urlencode "after=$cursor"})"
  jq -e '.data' >/dev/null 2>&1 <<<"$page" || { echo "Bad list response: ${page:0:200}" >&2; exit 1; }
  jq -c '.data[]' <<<"$page" >> "$TMP/list.ndjson"
  cursor="$(jq -r '.next_cursor // empty' <<<"$page")"
  [[ -z "$cursor" ]] && break
done
touch "$TMP/list.ndjson"

# project fields from the JSON-encoded description (guarded — can be null/garbage)
jq -c '
  ( (.description // .parameters["antithesis.description"] // "") as $r
    | (try ($r|fromjson) catch null) ) as $d
  | { run_id, status, created: .created_at,
      org:  ($d.testRun.repository.organization // "unknown"),
      repo: ($d.testRun.repository.repo // "unknown"),
      dir:  ($d.testRun.directory // "unknown"),
      requester: ($d.testRun.requester // "unknown") }
' "$TMP/list.ndjson" > "$TMP/runs.ndjson"
total=$(wc -l < "$TMP/runs.ndjson")
echo "Found $total runs." >&2

# ------------------------------------------------- 2. per-run detail (parallel)
# Incomplete: classify via failure_reason. Completed: prefetch failing properties.
process_run() {
  local id="$1" status="$2" out="$TMP/runs/$1.json"

  if [[ "$status" == "incomplete" ]]; then
    local detail reason ih vt report cls evidence
    detail="$(fetch "$BASE/api/v1/runs/$id")" || { echo '{"error":"detail fetch failed"}' > "$out"; return; }
    reason="$(jq -r '.failure_reason // ""' <<<"$detail")"
    ih="$(jq -r '.failure_moment.input_hash // ""' <<<"$detail")"
    vt="$(jq -r '.failure_moment.vtime // ""' <<<"$detail")"
    report="$(jq -r 'if .links.triage_report then "yes" else "no" end' <<<"$detail")"
    cls="" evidence=""

    case "$reason" in
      "Antithesis session failed")
        cls="ANTITHESIS" ;;                          # platform; nothing else to fetch
      "Build failed")
        # root cause = terminal [STATUS] line of build-logs (4xxx client / 3xxx antithesis)
        evidence="$(fetch "$BASE/api/v1/runs/$id/build_logs" \
                     | jq -r '.text // empty' | grep -F '[STATUS]' | tail -1 || true)"
        local code
        code="$(grep -o '"code":[0-9]*' <<<"$evidence" | head -1 | tr -dc '0-9' || true)"
        case "$code" in
          4*) cls="CLIENT_BUILD" ;;
          3*) cls="ANTITHESIS" ;;
          *)  cls="BUILD_FAILED_UNPARSED" ;;
        esac ;;
      "Container setup failure"|"No 'setup complete' event received")
        cls="CLIENT_SETUP" ;;                        # real moment attached; logs on demand
      "")
        cls="UNKNOWN" ;;                             # no reason, no moment: unresolved
      *)
        cls="OTHER" ;;                               # take reason at face value
    esac

    jq -n --arg cls "$cls" --arg reason "$reason" --arg ih "$ih" --arg vt "$vt" \
          --arg report "$report" --arg evidence "$evidence" '
      { class: $cls, failure_reason: (if $reason=="" then null else $reason end),
        failure_moment: (if $ih=="" then null else {input_hash:$ih, vtime:$vt} end),
        report_exists: ($report=="yes"),
        evidence: (if $evidence=="" then null else $evidence end) }' > "$out"

  elif [[ "$status" == "completed" ]]; then
    local cursor="" page props="[]"
    while :; do
      page="$(fetch -G "$BASE/api/v1/runs/$id/properties" \
                --data-urlencode "status=Failing" \
                ${cursor:+--data-urlencode "after=$cursor"})" || break
      jq -e '.data' >/dev/null 2>&1 <<<"$page" || break
      props="$(jq -c --argjson acc "$props" '$acc + .data' <<<"$page")"
      cursor="$(jq -r '.next_cursor // empty' <<<"$page")"
      [[ -z "$cursor" ]] && break
    done
    # compact: name/group/counts + up to 3 counterexample moments (event props)
    # + a truncated first counterexample sample (non-event props)
    jq -c '
      { failing_count: length,
        failing: [ .[] | {
          name, group: (.group // null), is_event,
          counterexample_count: (.counterexample_count // null),
          moments: ([ (.counterexamples // [])[]? | objects | .moment? // empty ] | .[0:3]),
          sample: ((.counterexamples // [])[0]? | if . == null then null else (tostring | .[0:200]) end)
        } ] }' <<<"$props" > "$out"
  fi
}
export -f process_run fetch
export TMP BASE

jq -r 'select(.status=="incomplete" or .status=="completed") | "\(.run_id)\t\(.status)"' \
   "$TMP/runs.ndjson" \
| xargs -P "$JOBS" -L 1 bash -c 'process_run "$1" "$2"' _ 2>"$TMP/errors.log" || true
[[ -s "$TMP/errors.log" ]] && { echo "Per-run fetch errors:" >&2; sort -u "$TMP/errors.log" | head >&2; }

# ---------------------------------------------------------------- 3. assemble
for f in "$TMP"/runs/*.json; do
  [[ -e "$f" ]] || break
  id="$(basename "$f" .json)"
  jq -c --arg id "$id" '{run_id:$id, detail:.}' "$f"
done > "$TMP/details.ndjson"
touch "$TMP/details.ndjson"

jq -n --arg from "$FROM" --arg to "$TO" --arg base "$BASE" \
      --slurpfile runs <(jq -c . "$TMP/runs.ndjson") \
      --slurpfile det  <(jq -c . "$TMP/details.ndjson") '
  ($det | map({(.run_id): .detail}) | add // {}) as $d
  | ($runs | map(. + {detail: ($d[.run_id] // null)})) as $all
  | { window: {from:$from, to:$to}, tenant_base:$base,
      generated_at: (now | todate),
      total_runs: ($all|length),
      projects: ( $all | group_by([.org,.repo,.dir]) | map(
        { org: .[0].org, repo: .[0].repo, dir: .[0].dir,
          requesters: (map(.requester)|unique),
          counts: (group_by(.status) | map({(.[0].status): length}) | add),
          in_progress_ids: [ .[] | select(.status=="in_progress") | .run_id ],
          cancelled_ids:   [ .[] | select(.status=="cancelled")   | .run_id ],
          clean_completed_ids: [ .[] | select(.status=="completed"
                                       and (.detail.failing_count // 0)==0) | .run_id ],
          completed_failing: [ .[] | select(.status=="completed"
                                       and (.detail.failing_count // 0) > 0)
            | {run_id, created, failing: .detail.failing} ],
          incomplete: [ .[] | select(.status=="incomplete")
            | {run_id, created} + (.detail // {class:"FETCH_FAILED"}) ]
        }) | sort_by(-((.counts.incomplete // 0)*100 +
                       ([.completed_failing[]?]|length))) ) }
' > "$OUT"

# ---------------------------------------------------------- 4. human summary
echo
jq -r '
  ["PROJECT","OWNER","RUNS","CLEAN","FAILING","INCOMPLETE","INCOMPLETE CLASSES"],
  ( .projects[] |
    [ "\(.org)/\(.repo):\(.dir)",
      (.requesters|join(",")),
      ([.counts[]?]|add|tostring),
      (.clean_completed_ids|length|tostring),
      (.completed_failing|length|tostring),
      ((.counts.incomplete // 0)|tostring),
      ( [.incomplete[].class] | group_by(.) | map("\(.[0]) x\(length)") | join(" ")
        | if .=="" then "-" else . end ) ] )
  | @tsv' "$OUT" | awk -F'\t' '
    { for (i=1;i<=NF;i++) { c[NR,i]=$i; if (length($i)>w[i]) w[i]=length($i) } n=NF; r=NR }
    END { for (i=1;i<=r;i++) { for (j=1;j<=n;j++) printf "%-*s  ", w[j], c[i,j]; print "" } }'
echo
echo "Wrote $OUT ($(wc -c < "$OUT") bytes)." >&2
