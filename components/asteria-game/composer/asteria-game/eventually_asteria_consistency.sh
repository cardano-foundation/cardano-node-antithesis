#!/usr/bin/env bash
# eventually_asteria_consistency.sh — post-workload consistency snapshot.
#
# Was finally_asteria_consistency.sh. Moved to the eventually_ category
# because finally_ commands stopped running to completion at config
# version cfg=56: Antithesis masks the finally_ set to weight 0
# (`masked_for_rollout`), so no timeline ever runs them to completion
# and the built-in "All commands were run to completion at least once"
# property trips. eventually_ commands complete reliably on this
# testnet (eventually_alive passes every run), so the consistency
# invariant is checked here instead. The check itself is unchanged:
# one snapshot pass that the asteria UTxO's @ship_counter@ equals the
# count of @SHIP*@ tokens at the spacetime spend address. Emits a
# single 'sdkSometimes' assertion (true after pure-spawn flows; false
# after quit/mine burns ships).
#
# Category note: eventually_ stops fault injection and kills the
# parallel drivers before this runs, so the players have stopped and
# the chain quiesces quickly — a short settle is enough.
#
# Budget: 5 s settle + 12 s binary cap + 2 s SIGKILL grace = 19 s
# worst case. Sized down from the finally_ budget (42 s) to fit the
# eventually_ per-command cap, which is tighter than finally_'s (~54 s)
# and undocumented — eventually_alive uses an 11 s envelope. The
# invariant binary returns in a few seconds in the healthy post-fault
# window, so the typical run is ~7 s; "run to completion at least once"
# only needs one such timeline. The 12 s `timeout` is the partition
# safety bound (N2C handshake can hang), with `--kill-after=2`
# escalating to SIGKILL so a slow-cleanup path can't outlive the
# deadline and exit rc=1, which sdk_run_signal_safe does not absorb.
# See #145.
#
# Single-shot per the antithesis-tests skill rule: short-lived
# eventually/finally only — no polling loops here.

set -u

# shellcheck disable=SC1091
source "$(dirname "$0")/helper_sdk.sh"

# Absorb in-bash signals (SIGTERM/SIGINT/SIGPIPE) into a silent
# observation + exit 0; defense in depth around sdk_run_signal_safe.
# See #142.
sdk_install_signal_trap "asteria_game eventually_consistency signal"

sleep 5
export ASTERIA_INVARIANT=consistency
sdk_run_signal_safe "asteria_game eventually_consistency container_stopped" \
    timeout --kill-after=2 12 /bin/asteria-invariant
