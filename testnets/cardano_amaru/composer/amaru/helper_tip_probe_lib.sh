#!/usr/bin/env bash
# Shared producer-tip probing for convergence composer commands.
#
# The helper_ prefix is intentional: Antithesis Composer ignores support
# files with this prefix when validating command names.
#
# Populates:
#   TIP_DETAILS   all probe results, one object per producer
#   TIP_SUCCESSES successful producer tip results
#   TIP_FAILURES  failed producer probe results
#   TIP_COUNT     number of successful producer tip responses
#   TIP_DISTINCT  number of distinct hashes among successful responses
#   TIP_FAILURE_REASONS unique failure reasons for failed producer probes
#   TIP_FAILURE_KIND classification for the latest result set

POOLS="${POOLS:-3}"
PORT="${PORT:-3001}"
TIP_DETAIL_LIMIT="${TIP_DETAIL_LIMIT:-600}"

build_tip_nodes() {
    TIP_NODES=()
    for i in $(seq 1 "$POOLS"); do
        TIP_NODES+=("p${i}")
    done
}

probe_result_json() {
    local node="$1"
    local stdout_file="$2"
    local stderr_file="$3"
    local status_file="$4"
    local exit_code

    if [ -f "$status_file" ]; then
        exit_code="$(cat "$status_file")"
    else
        exit_code="999"
    fi

    jq -n -c \
        --arg node "$node" \
        --arg exitCode "$exit_code" \
        --argjson limit "$TIP_DETAIL_LIMIT" \
        --rawfile stdout "$stdout_file" \
        --rawfile stderr "$stderr_file" '
        def clean($s): $s | gsub("[\r\n\t]+"; " ");
        def bounded($s):
            (clean($s)) as $cleaned
            | if ($cleaned | length) > $limit then
                ($cleaned[0:$limit] + "...<truncated>")
              else
                $cleaned
              end;
        def reason($code; $stderr; $stdout; $parsed):
            (clean($stderr + " " + $stdout)) as $output
            | if $code != 0 then
                if $output | test("PingClientFindIntersectDeserialiseFailure") then
                  "tip_protocol_deserialise_failure"
                elif $output | test("getAddrInfo|Name or service not known|Temporary failure in name resolution") then
                  "name_resolution_failed"
                elif $output | test("Connection refused") then
                  "connection_refused"
                elif $output | test("No route to host") then
                  "no_route_to_host"
                elif $output | test("Connection timed out|timed out") then
                  "connection_timed_out"
                elif $output | test("HandshakeError|Handshake") then
                  "protocol_handshake_failed"
                else
                  "ping_failed"
                end
              elif $parsed == null then "invalid_json"
              else "missing_tip"
              end;

        ($exitCode | tonumber? // 999) as $code
        | (try ($stdout | fromjson) catch null) as $parsed
        | if $code == 0 and $parsed != null and ($parsed.tip[0]? != null) then
            {
              node: $node,
              ok: true,
              exit_code: $code,
              hash: $parsed.tip[0].hash,
              block: $parsed.tip[0].blockNo,
              slot: $parsed.tip[0].slotNo
            }
          else
            {
              node: $node,
              ok: false,
              exit_code: $code,
              reason: reason($code; $stderr; $stdout; $parsed),
              stderr: bounded($stderr),
              stdout: bounded($stdout)
            }
          end'
}

probe_all_tips() {
    local temp_dir node
    temp_dir="$(mktemp -d)"

    build_tip_nodes

    for node in "${TIP_NODES[@]}"; do
        (
            cardano-cli ping -j \
                --magic 42 --host "${node}.example" --port "$PORT" \
                --tip --quiet -c1 \
                >"$temp_dir/${node}.stdout" \
                2>"$temp_dir/${node}.stderr"
            printf '%s' "$?" >"$temp_dir/${node}.status"
        ) &
    done
    wait || true

    TIP_DETAILS="$(
        for node in "${TIP_NODES[@]}"; do
            probe_result_json \
                "$node" \
                "$temp_dir/${node}.stdout" \
                "$temp_dir/${node}.stderr" \
                "$temp_dir/${node}.status"
        done | jq -sc '.'
    )"
    rm -rf "$temp_dir"

    TIP_SUCCESSES="$(printf '%s' "$TIP_DETAILS" | jq -c '[.[] | select(.ok == true)]')"
    TIP_FAILURES="$(printf '%s' "$TIP_DETAILS" | jq -c '[.[] | select(.ok != true)]')"
    TIP_COUNT="$(printf '%s' "$TIP_SUCCESSES" | jq 'length')"
    TIP_DISTINCT="$(printf '%s' "$TIP_SUCCESSES" | jq '[.[].hash] | unique | length')"
    TIP_FAILURE_REASONS="$(printf '%s' "$TIP_FAILURES" | jq -c '[.[].reason] | sort | unique')"

    classify_tip_probe_result
}

all_failures_have_reason() {
    local reason="$1"
    [ "$(printf '%s' "$TIP_FAILURE_REASONS" | jq --arg reason "$reason" 'length == 1 and .[0] == $reason')" = "true" ]
}

classify_tip_probe_result() {
    if [ "$TIP_COUNT" = "$POOLS" ] && [ "$TIP_DISTINCT" = "1" ]; then
        TIP_FAILURE_KIND="tips_agree"
    elif [ "$TIP_COUNT" = "0" ]; then
        if all_failures_have_reason "tip_protocol_deserialise_failure"; then
            TIP_FAILURE_KIND="no_tip_protocol_success"
        elif all_failures_have_reason "name_resolution_failed"; then
            TIP_FAILURE_KIND="no_producers_resolved"
        elif all_failures_have_reason "connection_refused" ||
             all_failures_have_reason "no_route_to_host" ||
             all_failures_have_reason "connection_timed_out"; then
            TIP_FAILURE_KIND="no_producer_connections"
        else
            TIP_FAILURE_KIND="no_producer_tips_returned"
        fi
    elif [ "$TIP_COUNT" != "$POOLS" ]; then
        if all_failures_have_reason "tip_protocol_deserialise_failure"; then
            TIP_FAILURE_KIND="some_tip_protocol_failures"
        elif all_failures_have_reason "name_resolution_failed"; then
            TIP_FAILURE_KIND="some_producers_unresolved"
        elif all_failures_have_reason "connection_refused" ||
             all_failures_have_reason "no_route_to_host" ||
             all_failures_have_reason "connection_timed_out"; then
            TIP_FAILURE_KIND="some_producer_connections_failed"
        else
            TIP_FAILURE_KIND="some_producer_tips_missing"
        fi
    else
        TIP_FAILURE_KIND="tips_divergent"
    fi
}
