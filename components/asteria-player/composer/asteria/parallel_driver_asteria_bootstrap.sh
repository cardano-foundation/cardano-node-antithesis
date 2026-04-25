#!/usr/bin/env bash
# parallel_driver_asteria_bootstrap.sh — runs the one-shot asteria
# bootstrap binary inside the asteria-player container.
#
# In iteration 1 this just exec's `asteria-bootstrap` which emits an
# sdk_reachable event and exits 0. Subsequent iterations replace
# the binary with code that:
#   - mints the admin NFT,
#   - locks the asteria UTxO with inline AsteriaDatum,
#   - deploys the four Aiken validators as ref-scripts,
#   - mints initial pellet UTxOs at known coordinates.
#
# Antithesis runs each `parallel_driver_*.sh` in parallel; the
# bootstrap is the first one to fire and the players' driver script
# blocks until this exits 0 (via docker-compose service_completed_successfully).

set -euo pipefail
SHELL="/bin/bash"
PATH="/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin:$PATH"

source "$(dirname "$0")/helper_sdk_lib.sh"

sdk_reachable "asteria_bootstrap_driver_started"

exec asteria-bootstrap
