#!/usr/bin/env bash
# helper_sdk.sh — asteria-game-specific composer helpers.
#
# Sources the shared SDK helper (emit functions + signal-safety
# primitives) from helper_sdk_common.sh, which the image build copies
# in alongside this file. Asteria-game has no component-specific
# helpers right now; all asteria-game scripts use the shared
# primitives directly (sdk_install_signal_trap, sdk_run_signal_safe,
# sdk_run_signal_safe_fn) and call their /bin/asteria-* binaries
# without a wrapper here.
#
# Files in /opt/antithesis/test/v1/<template>/ whose basename starts
# with "helper_" are ignored by the composer, so this file is never
# invoked as a test command. It's sourced by sibling commands.

source "$(dirname "$0")/helper_sdk_common.sh"
