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
export ASTERIA_INVARIANT=admin_singleton
exec /bin/asteria-invariant
