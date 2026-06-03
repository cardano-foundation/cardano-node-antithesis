#!/usr/bin/env bash
# helper_sdk.sh — shell emitter for the Antithesis Fallback SDK.
#
# Files prefixed with `helper_` are ignored by the Antithesis composer
# scheduler, so this file is never executed as a test command. Sibling
# scripts source it.
#
# Each emit appends one assertion event to $ANTITHESIS_OUTPUT_DIR/sdk.jsonl
# (default /tmp/sdk.jsonl). Antithesis reads that file to score the run.

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
         }}' >> "$dir/sdk.jsonl" 2>/dev/null || true
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
    local cond=false; [ "$1" = "true" ] && cond=true
    _sdk_emit "Sometimes" "sometimes" "$cond" true "$2" "$2" "${3:-null}"
}

# sdk_always <true|false> <id> [details_json]
sdk_always() {
    local cond=false; [ "$1" = "true" ] && cond=true
    _sdk_emit "Always" "always" "$cond" true "$2" "$2" "${3:-null}"
}

# sdk_run_signal_safe <sig_id> <binary> [args...]
#
# Run a binary and absorb signal-induced non-zero exits into an
# sdk_unreachable signal + exit 0. Antithesis applies node/stop and
# node/kill faults; a cardano-cli call interrupted mid-flight exits
# with a signal-induced code and would otherwise trip the built-in
# "Always: Commands finish with zero exit code" property.
sdk_run_signal_safe() {
    local sig_id="$1"; shift
    "$@"
    local rc=$?
    case "$rc" in
        0) return 0 ;;
        # 128+9=137 SIGKILL, 128+15=143 SIGTERM, 128+1=129 SIGHUP,
        # 124 timeout(1) wrapper, 255 generic abort/exec-failed
        129 | 137 | 143 | 124 | 255)
            sdk_unreachable "$sig_id" \
                "$(jq -nc --argjson r "$rc" \
                    '{rc:$r, reason:"process exited with signal-induced code; container likely stopped mid-run"}')"
            return 0
            ;;
        *) return "$rc" ;;
    esac
}

# sdk_setup_complete [details_json]
#
# Emit the Antithesis lifecycle "setup complete" signal: tells the
# hypervisor the system is healthy and it may START injecting faults.
# Place it where the cluster is confirmed up (e.g. once relay1's tip is
# advancing), NOT inside a first_ command (those run after this signal).
sdk_setup_complete() {
    local dir; dir="$(_sdk_output_dir)"
    mkdir -p "$dir"
    jq -nc --argjson d "${1:-null}" \
        '{antithesis_setup: {status: "complete", details: $d}}' \
        >> "$dir/sdk.jsonl" 2>/dev/null || true
}
