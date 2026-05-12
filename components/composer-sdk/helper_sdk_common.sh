#!/usr/bin/env bash
# helper_sdk_common.sh — shared Antithesis Fallback SDK helpers.
#
# Single source of truth for the SDK emit functions and the bash-level
# signal-handling primitives used by every component's composer
# scripts. Component-specific helpers (e.g. tx-generator's
# control_socket_request) live in the component's own helper_*.sh,
# which sources this file.
#
# Files in /opt/antithesis/test/v1/<template>/ whose basename starts
# with "helper_" are ignored by the composer, so this file is never
# invoked as a test command. The image build copies it into the same
# directory as the component scripts, alongside the per-component
# helper, so the existing `source "$(dirname "$0")/helper_*.sh"`
# pattern resolves correctly.
#
# Each emit appends one assertion event to $ANTITHESIS_OUTPUT_DIR/sdk.jsonl
# (default /tmp/sdk.jsonl). Antithesis reads that file to score the run.

_sdk_output_dir() { printf '%s' "${ANTITHESIS_OUTPUT_DIR:-/tmp}"; }

_sdk_emit() {
    # args: display_type assert_type condition hit id message details_json [must_hit]
    local display_type="$1" assert_type="$2" condition="$3"
    local hit="$4" id="$5" msg="$6" details_json="${7:-null}" must_hit="${8:-true}"
    local dir
    dir="$(_sdk_output_dir)"
    mkdir -p "$dir"
    # flock the open append-FD so concurrent parallel_driver_*
    # invocations can't interleave at the syscall level under
    # Antithesis FS-fault injection. O_APPEND alone is atomic for
    # small writes on stable kernels, but pathological hangs have
    # been observed on shared sdk.jsonl writes when the platform
    # throttles fs syscalls; the lock makes the contention explicit
    # and bounded.
    {
        flock -x 9
        jq -nc \
            --arg id "$id" \
            --arg msg "$msg" \
            --arg dt "$display_type" \
            --arg at "$assert_type" \
            --argjson cond "$condition" \
            --argjson hit "$hit" \
            --argjson details "$details_json" \
            --argjson must_hit "$must_hit" \
            '{antithesis_assert: {
                id: $id,
                message: $msg,
                condition: $cond,
                display_type: $dt,
                hit: $hit,
                must_hit: $must_hit,
                assert_type: $at,
                location: {file:"", function:"", class:"", begin_line:0, begin_column:0},
                details: $details
             }}' >&9
    } 9>>"$dir/sdk.jsonl"
}

# sdk_reachable <id> [details_json]
sdk_reachable()   { _sdk_emit "Reachable"           "reachability" true  true "$1" "$1" "${2:-null}"; }

# sdk_unreachable <id> [details_json]
sdk_unreachable() { _sdk_emit "AlwaysOrUnreachable" "always"       false true "$1" "$1" "${2:-null}"; }

# sdk_sometimes <true|false> <id> [details_json]
sdk_sometimes() {
    local cond=false; [ "$1" = "true" ] && cond=true
    _sdk_emit "Sometimes" "sometimes" "$cond" true "$2" "$2" "${3:-null}"
}

# sdk_sometimes_optional <true|false> <id> [details_json]
#
# Same shape as sdk_sometimes but with must_hit:false — observational
# coverage, never a finding. Use this for branches where condition:true
# is not guaranteed reachable across the explored timelines (signal
# handlers, cold-start probes). AlwaysOrUnreachable with hit:true +
# condition:false IS a finding, while sdk_sometimes_optional false is
# silent.
sdk_sometimes_optional() {
    local cond=false; [ "$1" = "true" ] && cond=true
    _sdk_emit "Sometimes" "sometimes" "$cond" true "$2" "$2" "${3:-null}" false
}

# sdk_always <true|false> <id> [details_json]
sdk_always() {
    local cond=false; [ "$1" = "true" ] && cond=true
    _sdk_emit "Always" "always" "$cond" true "$2" "$2" "${3:-null}"
}

# sdk_run_signal_safe <sig_id> <binary> [args...]
#
# Run a binary and absorb signal-induced non-zero exits into a
# sdk_sometimes_optional false event + exit 0. Antithesis applies node
# faults (stop/kill) to component containers mid-run, and callers wrap
# binaries with timeout(1) to bound execution under composer's
# per-command cap. Either path produces signal-induced exit codes
# (137 SIGKILL, 143 SIGTERM, 124 timeout, 255 abort) that aren't real
# test failures — the composer's "Always: Commands finish with zero
# exit code" property would otherwise flag them as bugs.
#
# Real non-zero exits (anything other than the signal-induced set)
# propagate unchanged so genuine binary errors still surface.
sdk_run_signal_safe() {
    local sig_id="$1"; shift
    "$@"
    local rc=$?
    case "$rc" in
        0) return 0 ;;
        # 128+9=137 SIGKILL
        # 128+15=143 SIGTERM
        # 128+1=129 SIGHUP
        # 124 timeout(1) wrapper
        # 255 generic abort / exec failed mid-call
        129 | 137 | 143 | 124 | 255)
            sdk_sometimes_optional false "$sig_id" \
                "$(jq -nc --argjson r "$rc" \
                    '{rc:$r, reason:"process exited with signal-induced code; container stopped mid-run or timeout(1) deadline hit"}')"
            return 0
            ;;
        *) return "$rc" ;;
    esac
}

# sdk_run_signal_safe_fn <sig_id> <fn_name>
#
# Same absorption contract as sdk_run_signal_safe, but for an entire
# shell function body — needed for scripts whose work is a multi-stage
# pipeline (printf | timeout 1 socat | jq), not a single binary call.
# Caller defines a function and passes its name; the function runs in
# the current shell (so it sees outer locals + traps) and its overall
# exit status flows through the same case statement.
sdk_run_signal_safe_fn() {
    local sig_id="$1" fn="$2"
    "$fn"
    local rc=$?
    case "$rc" in
        0) return 0 ;;
        129 | 137 | 143 | 124 | 255)
            sdk_sometimes_optional false "$sig_id" \
                "$(jq -nc --argjson r "$rc" \
                    '{rc:$r, reason:"shell pipeline exited with signal-induced code; container stopped mid-run or timeout(1) deadline hit"}')"
            return 0
            ;;
        *) return "$rc" ;;
    esac
}

# sdk_install_signal_trap <sig_id>
#
# Catches SIGTERM/SIGINT/SIGPIPE that arrive at the bash interpreter
# itself (not the wrapped binary) and converts them into a silent
# observation + exit 0. Antithesis fault injection can deliver these
# signals to any process at any time; without a trap, bash dies with
# 128+sig and the composer's "Always: zero exit code" property fires
# even though the script was about to honour its "exit 0 always"
# contract. SIGPIPE in particular is delivered when any command in
# the script tries to write to a pipe whose reader has closed — for
# example the SDK's antithesis_random Rust binary writing to a closed
# command-substitution pipe, producing
#   Error: Os { code: 32, kind: BrokenPipe, message: "Broken pipe" }
# on the script's stderr immediately before bash exits with 141.
#
# Idempotent: safe to call once at the top of every script
# right after sourcing the helper.
sdk_install_signal_trap() {
    local sig_id="$1"
    # SC2064 disabled deliberately: we *want* "$sig_id" to expand
    # NOW (at trap installation), so each script binds the trap to
    # its own static id; the second arg is a literal signal name
    # that doesn't need late expansion.
    # shellcheck disable=SC2064
    trap "_sdk_handle_trap '$sig_id' TERM" SIGTERM
    # shellcheck disable=SC2064
    trap "_sdk_handle_trap '$sig_id' INT"  SIGINT
    # shellcheck disable=SC2064
    trap "_sdk_handle_trap '$sig_id' PIPE" SIGPIPE
}

# Internal — invoked by the trap installed above. Best-effort emit of a
# Sometimes-optional false event, then exit 0. Tolerates jq failure
# under fault injection so the exit-0 contract holds even when the
# platform is actively sabotaging fs/process syscalls.
_sdk_handle_trap() {
    local sig_id="$1" sig="$2"
    sdk_sometimes_optional false "$sig_id" \
        "$(jq -nc --arg sig "$sig" \
            '{killed_by_signal:$sig, reason:"bash interpreter received signal under fault injection"}' \
            2>/dev/null \
         || printf '%s' 'null')"
    exit 0
}
