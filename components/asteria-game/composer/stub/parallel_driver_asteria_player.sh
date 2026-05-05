#!/usr/bin/env bash
# parallel_driver_asteria_player.sh — single-pass invocation of the
# /bin/asteria-game player loop.
#
# Antithesis schedules parallel_driver_* concurrently with each
# other; this script does one observe+spawn-attempt pass and exits 0.
# Re-firing is the composer's job, so the binary itself does not
# loop — see PlayerMain.hs.
#
# ASTERIA_PLAYER_ID picks which player attempts the spawn. We rotate
# through 1..3 based on the wallclock so different timelines exercise
# different players. Player 1 is the spawn-eligible player today
# (PlayerMain gates spawn on playerId == "1"); the others observe
# the asteria UTxO without acting, exercising the read path.

set -u

# shellcheck disable=SC1091
source "$(dirname "$0")/helper_sdk.sh"

PLAYER_ID="$(( ($(date +%s) % 3) + 1 ))"
export ASTERIA_PLAYER_ID="$PLAYER_ID"
sdk_run_signal_safe "stub asteria_player_${PLAYER_ID} container_stopped" \
    /bin/asteria-game
