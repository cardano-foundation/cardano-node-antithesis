#!/usr/bin/env bash
# finally_governance_summary.sh — end-of-run coverage marker.
#
# Emits a single Sometimes/Reachable pair proving the gov-cli
# lifecycle reached end-of-test. If this row never passes, the
# container was killed early and the other governance_* signals in the
# run are suspect.
#
# No `set -e`, no sleep, no node calls: fault injection can deliver
# SIGTERM mid-script, and this marker must always exit 0.

OUT="${ANTITHESIS_OUTPUT_DIR:-/tmp}/sdk.jsonl"
mkdir -p "$(dirname "$OUT")" 2>/dev/null

jq -nc '{
  antithesis_assert: {
    id:           "governance_run_completed",
    message:      "governance_run_completed",
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
    id:           "finally_governance_summary entered",
    message:      "finally_governance_summary entered",
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
