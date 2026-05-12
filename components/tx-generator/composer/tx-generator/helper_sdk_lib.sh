#!/usr/bin/env bash
# helper_sdk_lib.sh — tx-generator-specific composer helpers.
#
# Sources the shared SDK helper (emit functions + signal-safety
# primitives) from helper_sdk_common.sh, which the image build copies
# in alongside this file. Anything tx-generator-specific (currently
# just the control-socket request shape) lives here.
#
# Files in /opt/antithesis/test/v1/<template>/ whose basename starts
# with "helper_" are ignored by the composer, so this file is never
# invoked as a test command. It's sourced by sibling commands.

source "$(dirname "$0")/helper_sdk_common.sh"

# control_socket_request <socket> <json-request>
#
# Keep nc as timeout's direct child. The older
# `timeout sh -c "printf ... | nc ..."` shape could leave the
# pipeline alive past the composer's command deadline under faulted
# socket behavior, surfacing as a built-in zero-exit failure.
control_socket_request() {
    local socket="$1" request="$2"
    printf '%s\n' "$request" \
        | timeout --kill-after=2s 5s nc -U -q 1 "$socket" 2>/dev/null \
        || true
}
