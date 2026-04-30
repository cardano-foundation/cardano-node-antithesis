#!/usr/bin/env bash
# helper_sdk_lib.sh — shell emitter for the Antithesis Fallback SDK.
#
# Files in /opt/antithesis/test/v1/<template>/ whose basename starts
# with "helper_" are ignored by the composer, so this file is never
# invoked as a test command. It's sourced by sibling commands.
#
# Writes one assertion event per call to $ANTITHESIS_OUTPUT_DIR/sdk.jsonl
# (default /tmp/sdk.jsonl). Antithesis reads that file to score the run.
#
# Usage:
#   source "$(dirname "$0")/helper_sdk_lib.sh"
#   sdk_reachable "health check started"
#   sdk_sometimes  true  "check succeeded"
#   sdk_unreachable "divergence observed" '{"node":"p1"}'
#   sdk_always     true  "tip agreed across nodes" '{"hosts":3}'

_sdk_output_dir() { printf '%s' "${ANTITHESIS_OUTPUT_DIR:-/tmp}"; }

_sdk_emit() {
    # args: display_type assert_type condition hit id message details_json
    local display_type="$1" assert_type="$2" condition="$3"
    local hit="$4" id="$5" msg="$6" details_json="${7:-null}"
    local dir
    dir="$(_sdk_output_dir)"
    mkdir -p "$dir"
    jq -nc \
        --arg id "$id" \
        --arg msg "$msg" \
        --arg dt "$display_type" \
        --arg at "$assert_type" \
        --argjson cond "$condition" \
        --argjson hit "$hit" \
        --argjson details "$details_json" \
        '{antithesis_assert: {
            id: $id,
            message: $msg,
            condition: $cond,
            display_type: $dt,
            hit: $hit,
            must_hit: true,
            assert_type: $at,
            location: {file:"", function:"", class:"", begin_line:0, begin_column:0},
            details: $details
         }}' >> "$dir/sdk.jsonl"
}

# sdk_reachable <id> [details_json]
sdk_reachable() {
    _sdk_emit "Reachable" "reachability" true true "$1" "$1" "${2:-null}"
}

# sdk_unreachable <id> [details_json]
sdk_unreachable() {
    _sdk_emit "AlwaysOrUnreachable" "always" false true "$1" "$1" "${2:-null}"
}

# sdk_sometimes <true|false> <id> [details_json]
sdk_sometimes() {
    local cond_bool="$1" id="$2" details="${3:-null}"
    local cond=false
    [ "$cond_bool" = "true" ] && cond=true
    _sdk_emit "Sometimes" "sometimes" "$cond" true "$id" "$id" "$details"
}

# sdk_always <true|false> <id> [details_json]
sdk_always() {
    local cond_bool="$1" id="$2" details="${3:-null}"
    local cond=false
    [ "$cond_bool" = "true" ] && cond=true
    _sdk_emit "Always" "always" "$cond" true "$id" "$id" "$details"
}

txgen_control_request() {
    local req="$1"
    local timeout="${TX_GEN_CONTROL_TIMEOUT:-55}"
    local err
    err="$(mktemp)"
    if rsp="$(printf '%s\n' "$req" \
        | nc -U -w "$timeout" "$CONTROL_SOCKET" 2>"$err")"; then
        rm -f "$err"
        printf '%s' "$rsp"
        return 0
    fi

    sdk_unreachable "tx_generator_control_request_failed" \
        "$(jq -nc \
            --arg socket "$CONTROL_SOCKET" \
            --arg error "$(cat "$err")" \
            '{socket:$socket,error:$error}')"
    rm -f "$err"
    return 1
}
