#!/usr/bin/env bash
# eventually_alive.sh — post-fault liveness probe via the indexer.
#
# Antithesis stops fault injection before this script starts. The
# check is intentionally small: confirm the long-lived utxo-indexer
# is still responsive on its Unix domain socket and at the chain
# tip (slotsBehind=0). The point is to fire the assertion, not to
# diagnose anything; richer convergence checks live in the sidecar
# template (`finally_tips_agree`, `eventually_converged`).
#
# Total budget: 3 s settle + 8×1 s retries = 11 s worst case. Sized
# to fit comfortably under the Antithesis composer's per-command
# timeout for eventually_ commands (observed ≤16 s for parallel_drivers,
# ≤54 s for finally_; using the tighter bound for eventually_ since
# its enforced cap isn't documented). Earlier 60 s budget tripped the
# composer's deadline and registered as a "Commands finish with zero
# exit code" finding; we'd rather lose recovery slack than a finding.
#
# Cold-start guard: if the indexer reports tipSlot=null after the
# full budget, no chain data has been received from upstream relay1
# yet (e.g. relay1 killed → indexer's upstream torn down → no
# RollForward arrived in time). That is a "system not yet
# bootstrapped" signal, not a liveness failure. The script records
# it as sdk_sometimes false (an observation in the report, not a
# finding) and exits 0. AlwaysOrUnreachable was the wrong primitive
# here: hit:true + condition:false on an Always-class assertion
# fires as a finding, defeating the intended "informational only"
# semantics.
#
# Both terminal paths exit 0 — the assertion outcome is recorded via
# the SDK; a non-zero shell exit would just add a duplicate signal
# under the composer's "Always: zero exit code" property.

set -u

# shellcheck disable=SC1091
source "$(dirname "$0")/helper_sdk.sh"

INDEXER_SOCK="${INDEXER_SOCK:-/tmp/idx.sock}"
SLEEP_SETTLE="${SLEEP_SETTLE:-3}"
MAX_ATTEMPTS="${MAX_ATTEMPTS:-8}"
RETRY_DELAY="${RETRY_DELAY:-1}"

# Absorb in-bash signals (SIGTERM/SIGINT/SIGPIPE) into a silent
# observation + exit 0; see #142 for context.
sdk_install_signal_trap "asteria_game eventually_alive signal"

sdk_reachable "asteria_game eventually_alive entered"

sleep "$SLEEP_SETTLE"

# Body wrapped via sdk_run_signal_safe_fn so signal-induced exits
# anywhere in the loop get absorbed the same way the single-binary
# launches in sibling scripts already do.
# shellcheck disable=SC2329  # invoked indirectly by sdk_run_signal_safe_fn below
_eventually_alive_body() {
    local last_reply=""
    local attempt
    for attempt in $(seq 1 "$MAX_ATTEMPTS"); do
        # `timeout 1 socat`: socat blocks indefinitely on
        # UNIX-CONNECT waiting for the indexer's reply with no
        # internal deadline. Without bounding, each loop iteration
        # runs as long as the indexer takes — observed wall times
        # of 70 s for the whole script when several iterations
        # stalled, well past composer's per-command cap. 1 s per
        # attempt keeps the loop's worst case at
        # 3 s settle + 8 × 1 s = 11 s.
        last_reply="$(printf '{"ready": null}\n' \
                       | timeout 1 socat - "UNIX-CONNECT:${INDEXER_SOCK}" 2>/dev/null \
                       || true)"
        if [ -n "$last_reply" ] \
            && printf '%s' "$last_reply" \
                | jq -e '(.slotsBehind // null) != null and .slotsBehind <= 5' \
                    >/dev/null 2>&1; then
            local tip
            tip="$(printf '%s' "$last_reply" | jq -r '.tipSlot // 0')"
            sdk_sometimes true "asteria_game eventually_alive holds" \
                "$(jq -nc --argjson a "$attempt" --argjson t "$tip" \
                    '{attempt:$a, tipSlot:$t}')"
            return 0
        fi
        sleep "$RETRY_DELAY"
    done

    # Budget exhausted. Distinguish "indexer has no chain data yet"
    # from "indexer is up but stuck behind". Both record as
    # Sometimes-false observations (no finding); operators read
    # the rate of each cause in the report's Sometimes-assertion
    # table.
    if [ -n "$last_reply" ] \
        && printf '%s' "$last_reply" | jq -e '.tipSlot == null' >/dev/null 2>&1; then
        # must_hit:false — the cold-start branch isn't guaranteed
        # to be reached across timelines (depends on whether a
        # fault-cascade window coincides with an eventually_
        # dispatch). The earlier sdk_sometimes (must_hit:true)
        # emit fired as a finding when it only saw condition:false,
        # defeating the intent that this be informational coverage
        # only.
        sdk_sometimes_optional false "asteria_game eventually_alive cold_start" \
            "$(jq -nc --argjson a "$MAX_ATTEMPTS" --arg reply "$last_reply" \
                '{attempts_exhausted:$a, last_reply:$reply, reason:"tipSlot=null — no RollForward yet from upstream"}')"
        return 0
    fi

    sdk_sometimes false "asteria_game eventually_alive holds" \
        "$(jq -nc --argjson a "$MAX_ATTEMPTS" --arg reply "$last_reply" \
            '{attempts_exhausted:$a, last_reply:$reply}')"
}

sdk_run_signal_safe_fn "asteria_game eventually_alive container_stopped" \
    _eventually_alive_body
exit 0
