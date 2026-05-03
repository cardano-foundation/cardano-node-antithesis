#!/usr/bin/env bash
#
# finally_adversary_summary.sh — emits a single end-of-run Sometimes
# assertion that proves the adversary lifecycle reached end-of-test.
#
# This is the test-level coverage signal: if the report's
# "adversary_run_completed" row is passed, the adversary container
# stayed alive long enough for the composer to fire its finally_*
# step. If the row never hits, the container was killed early and
# every other "adversary_*" Sometimes assertion in this run is
# suspect.
#
# Lifecycle notes:
# - No 'set -e'. Antithesis fault injection can deliver SIGTERM
#   mid-script (e.g. during sleep); with set -e the exit code from
#   the interrupted command propagates and the script dies non-zero,
#   tripping the "Always: Commands finish with zero exit code"
#   property. This script's only job is to emit one Sometimes event
#   per timeline; we always exit 0.
# - No sleep. The classic finally_/eventually_ pattern wants a settle
#   delay before checking system invariants, but this script doesn't
#   check anything — it just emits a coverage marker. Skipping the
#   sleep removes the largest interruption window.

OUT="${ANTITHESIS_OUTPUT_DIR:-/tmp}/sdk.jsonl"
mkdir -p "$(dirname "$OUT")" 2>/dev/null

# Two inline JSONL events. `|| true` keeps the script exit 0 even if
# jq or the file write fails — better to mis-emit than to flag a
# spurious "command exited non-zero" finding.
jq -nc '{
  antithesis_assert: {
    id:           "adversary_run_completed",
    message:      "adversary_run_completed",
    condition:    true,
    display_type: "Sometimes",
    hit:          true,
    must_hit:     true,
    assert_type:  "sometimes",
    location:     {file:"", function:"", class:"", begin_line:0, begin_column:0},
    details:      null
  }
}' >> "$OUT" 2>/dev/null || true

jq -nc '{
  antithesis_assert: {
    id:           "finally_adversary_summary entered",
    message:      "finally_adversary_summary entered",
    condition:    true,
    display_type: "Reachable",
    hit:          true,
    must_hit:     true,
    assert_type:  "reachability",
    location:     {file:"", function:"", class:"", begin_line:0, begin_column:0},
    details:      null
  }
}' >> "$OUT" 2>/dev/null || true

exit 0
