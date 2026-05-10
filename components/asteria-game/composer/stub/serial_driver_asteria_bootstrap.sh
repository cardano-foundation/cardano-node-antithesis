#!/usr/bin/env bash
# serial_driver_asteria_bootstrap.sh — one-shot, idempotent deploy
# of the asteria game state.
#
# Antithesis schedules serial_driver_* commands with exclusive
# access — while this script runs, no parallel_driver_ runs on
# this timeline. Bootstrap exits 0 quickly on subsequent
# invocations: the asteria-bootstrap binary queries the
# Provider's UTxOs at the asteria spend address and skips the
# mint+lock if a UTxO already carries the @asteriaAdmin@ token.
#
# Antithesis can restart the asteria-game container at any time,
# which re-fires this script. The contract is: re-running must be
# safe — see Asteria.Bootstrap.isAlreadyDeployed.

set -u

# shellcheck disable=SC1091
source "$(dirname "$0")/helper_sdk.sh"

# Absorb in-bash signals (SIGTERM/SIGINT/SIGPIPE) into a silent
# observation + exit 0; defense in depth around sdk_run_signal_safe.
# See #142.
sdk_install_signal_trap "stub asteria_bootstrap signal"

# `timeout 25` bounds the bootstrap binary; it's a serial_driver
# so its budget is at least as generous as parallel_driver. 25 s
# is conservative — first-time deploy needs to mint+lock the NFT
# (a few chain rounds) but subsequent invocations exit fast on the
# isAlreadyDeployed short-circuit. timeout 124 →
# sdk_run_signal_safe → no finding.
sdk_run_signal_safe "stub asteria_bootstrap container_stopped" \
    timeout 25 /bin/asteria-bootstrap
