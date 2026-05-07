#!/usr/bin/env bash
# finally_asteria_consistency.sh — end-of-run consistency snapshot.
#
# Antithesis schedules finally_* commands once at the end of the
# run, after fault injection has stopped. This driver waits a
# settle window so the chain can drain its mempool, then runs
# one snapshot pass of the consistency invariant: the asteria
# UTxO's @ship_counter@ equals the count of @SHIP*@ tokens at
# the spacetime spend address. Emits a single 'sdkSometimes'
# assertion (true after pure-spawn flows; false after quit/mine
# burns ships).
#
# Single-shot per the antithesis-tests skill rule: short-lived
# eventually/finally only — no polling loops here.

set -u

# shellcheck disable=SC1091
source "$(dirname "$0")/helper_sdk.sh"

# 10 s settle + 30 s binary cap = 40 s worst case, comfortably
# under composer's finally per-command cap (~54 s observed). The
# binary queries chain UTxOs and counts SHIP tokens; under
# partition it can hang on N2C handshake, hence the `timeout 30`
# bound. Earlier 15 s + unbounded binary blew past 54 s.
sleep 10
export ASTERIA_INVARIANT=consistency
sdk_run_signal_safe "stub finally_consistency container_stopped" \
    timeout 30 /bin/asteria-invariant
