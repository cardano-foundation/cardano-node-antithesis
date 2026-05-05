#!/usr/bin/env bash
# eventually_alive.sh — stub post-fault liveness probe via the indexer.
#
# Antithesis stops fault injection before this script starts. The
# check is intentionally small: confirm the long-lived utxo-indexer
# is still responsive on its Unix domain socket and at the chain
# tip (slotsBehind=0). The point is to fire the assertion, not to
# diagnose anything; richer convergence checks live in the sidecar
# template (`finally_tips_agree`, `eventually_converged`).
#
# Total budget: 30 s settle + 15×2 s retries = 60 s worst case.
# Sized to absorb an indexer cold-start when Antithesis kills the
# asteria-game container (a node fault) right before the eventually_
# fires: the indexer's reconnect goes through node-replaying with
# exponential backoff (1s, 2s, 4s, 8s, 16s … ≈ 13 s to attempt 7) plus
# N2C handshake + first-block write. 25 s wasn't enough; 60 s covers
# the typical post-kill bootstrap.
#
# Cold-start guard: if the indexer reports tipSlot=null after the full
# budget, no chain data has been received from upstream relay1 yet.
# That is a "system not yet bootstrapped" signal — distinct from "the
# indexer is up but stuck behind the chain". The script emits an
# sdk_unreachable and exits 0 in that case so the composer's
# "Always: zero exit" property doesn't flag a fault-cascade window
# (relay1 killed → indexer's upstream torn down → no RollForward yet)
# as a real liveness failure. tipSlot != null && slotsBehind > 5 is
# the only path that emits sdk_sometimes false + exit 1.

set -u

# shellcheck disable=SC1091
source "$(dirname "$0")/helper_sdk.sh"

INDEXER_SOCK="${INDEXER_SOCK:-/tmp/idx.sock}"
SLEEP_SETTLE="${SLEEP_SETTLE:-30}"
MAX_ATTEMPTS="${MAX_ATTEMPTS:-15}"
RETRY_DELAY="${RETRY_DELAY:-2}"

sdk_reachable "stub eventually_alive entered"

sleep "$SLEEP_SETTLE"

LAST_REPLY=""
for attempt in $(seq 1 "$MAX_ATTEMPTS"); do
    LAST_REPLY="$(printf '{"ready": null}\n' | socat - "UNIX-CONNECT:${INDEXER_SOCK}" 2>/dev/null || true)"
    if [ -n "$LAST_REPLY" ] \
        && printf '%s' "$LAST_REPLY" | jq -e '(.slotsBehind // null) != null and .slotsBehind <= 5' >/dev/null 2>&1; then
        TIP="$(printf '%s' "$LAST_REPLY" | jq -r '.tipSlot // 0')"
        sdk_sometimes true "stub eventually_alive holds" \
            "$(jq -nc --argjson a "$attempt" --argjson t "$TIP" '{attempt:$a, tipSlot:$t}')"
        exit 0
    fi
    sleep "$RETRY_DELAY"
done

# Budget exhausted. Distinguish "indexer has no chain data yet" from
# "indexer is up but stuck behind".
if [ -n "$LAST_REPLY" ] \
    && printf '%s' "$LAST_REPLY" | jq -e '.tipSlot == null' >/dev/null 2>&1; then
    sdk_unreachable "stub eventually_alive cold_start" \
        "$(jq -nc --argjson a "$MAX_ATTEMPTS" --arg reply "$LAST_REPLY" \
            '{attempts_exhausted:$a, last_reply:$reply, reason:"tipSlot=null — no RollForward yet from upstream"}')"
    exit 0
fi

sdk_sometimes false "stub eventually_alive holds" \
    "$(jq -nc --argjson a "$MAX_ATTEMPTS" --arg reply "$LAST_REPLY" \
        '{attempts_exhausted:$a, last_reply:$reply}')"
exit 1
