#!/usr/bin/env bash
# anytime_asteria_admin_singleton.sh — periodic invariant check.
#
# Antithesis schedules anytime_* commands at random points
# during the run. This driver runs one snapshot pass of the
# admin-singleton invariant: exactly one @asteriaAdmin@ NFT
# exists at the asteria spend address. Emits a single
# 'sdkAlways' assertion (hit=true on the happy path; hit=false
# if the count is anything other than 1).
#
# The bootstrap mints the NFT once and locks it; nothing in the
# designed game flow burns or duplicates it, so any deviation is
# a real bug.

set -u

# shellcheck disable=SC1091
source "$(dirname "$0")/helper_sdk.sh"

export ASTERIA_INVARIANT=admin_singleton
# `timeout 12` bounds the invariant-check binary under composer's
# anytime per-command cap. The binary queries the chain via N2C
# and can hang under partition; bound to 12 s with margin for the
# absorb path. timeout 124 → sdk_run_signal_safe → no finding.
sdk_run_signal_safe "stub anytime_admin_singleton container_stopped" \
    timeout 12 /bin/asteria-invariant
