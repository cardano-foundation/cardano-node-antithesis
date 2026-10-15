#!/usr/bin/env bash
# The public wiki carries the weekly summary only. Bug findings about the
# systems under test go privately to each project's owner, so the upload
# tool's check must refuse a page that carries one and accept a summary.
set -euo pipefail

repo_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
uploader="$repo_root/tools/antithesis-wiki-report/scripts/upload-report.sh"
tmp_root=$(mktemp -d)
trap 'rm -rf "$tmp_root"' EXIT

fail() {
  printf 'FAIL %s\n' "$*" >&2
  exit 1
}

pass() {
  printf 'PASS %s\n' "$1"
}

summary="$tmp_root/summary.md"
cat >"$summary" <<'EOF'
# Antithesis platform report — 2026-W41

**Analyzed window:** 2026-10-05 → 2026-10-12 (UTC) · 58 runs · 6 projects · tenant: amaru-cardano

[Antithesis-Reports](Antithesis-Reports)

## Projects at a glance

| Project | Owner | Runs | Clean | Incomplete | State |
|---|---|---|---|---|---|
| cardano-node-antithesis · testnets/cardano_node_master | cfhal | 28 | 20 | 0 | 🟠 Node bug |
| cardano-node-antithesis · testnets/cardano_node_head | cfhal | 7 | 6 | 0 | 🟦 Antithesis failure |

**🟦 Antithesis — No Antithesis errors failed in 1 run.** `fe222c247145a3186799298d5bd9a08b-62-14`
EOF

"$uploader" check "$summary" 2>"$tmp_root/summary.err" ||
  fail "a summary-only page was refused: $(cat "$tmp_root/summary.err")"
pass summary-page-accepted

# Each line is one way the report format carries a finding about the system
# under test; any one of them alone must make the page unpublishable.
findings=(
  '**🟥 Real — cardano-node container exited with code 1 after a fault-injected stop.**'
  '**🟨 Potential — cluster fork depth < k failed (once, at vtime 2183.2).**'
  'Pointer for cfhal: inspect the relay2/p3 node logs just before they died.'
  '<details><summary>Suggested fix</summary>'
)
for finding in "${findings[@]}"; do
  page="$tmp_root/finding.md"
  cp "$summary" "$page"
  printf '\n%s\n' "$finding" >>"$page"
  if "$uploader" check "$page" 2>"$tmp_root/finding.err"; then
    fail "a page carrying a finding was accepted: $finding"
  fi
  grep -Fq 'bug finding' "$tmp_root/finding.err" ||
    fail "a finding page was refused for the wrong reason: $(cat "$tmp_root/finding.err")"
done
pass finding-pages-refused

printf 'WIKI-REPORT-CHECK summary=accepted findings_refused=%d\n' "${#findings[@]}"
