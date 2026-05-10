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

# Absorb in-bash signals (SIGTERM/SIGINT/SIGPIPE) into a silent
# observation + exit 0; defense in depth around sdk_run_signal_safe.
# See #142.
sdk_install_signal_trap "stub asteria_player signal"

PLAYER_ID="$(( ($(date +%s) % 3) + 1 ))"
export ASTERIA_PLAYER_ID="$PLAYER_ID"
# `timeout 12 /bin/asteria-game` — the binary has no internal
# deadline and can hang on N2C handshake or chain-follower reads
# under cluster perturbation. Composer's parallel_driver per-command
# cap is ~16 s; bounding to 12 s gives margin for the bash trap +
# sdk_run_signal_safe emit. Timeout exits 124 → caught by
# sdk_run_signal_safe → recorded via sdk_sometimes_optional false
# (no finding).
sdk_run_signal_safe "stub asteria_player_${PLAYER_ID} container_stopped" \
    timeout 12 /bin/asteria-game
