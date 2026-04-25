#!/usr/bin/env bash
# parallel_driver_asteria_player.sh — long-running asteria player.
#
# Reads ASTERIA_PLAYER_ID from the environment (set per-replica in
# docker-compose). The Antithesis composer runs this script in
# parallel for each player container so the hypervisor explores the
# game state space across multiple actors simultaneously.

set -euo pipefail
SHELL="/bin/bash"
PATH="/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin:$PATH"

source "$(dirname "$0")/helper_sdk_lib.sh"

PLAYER_ID="${ASTERIA_PLAYER_ID:-unknown}"

sdk_reachable "asteria_player_driver_started_${PLAYER_ID}"

exec asteria-player
