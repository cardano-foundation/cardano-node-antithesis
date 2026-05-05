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

sdk_run_signal_safe "stub asteria_bootstrap container_stopped" \
    /bin/asteria-bootstrap
