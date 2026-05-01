#!/usr/bin/env bash

set -o pipefail

SHELL="/bin/bash"
PATH="/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin"

# Environment variables
CONVERGENCE_COMPOSER="${CONVERGENCE_COMPOSER:-true}"
POOLS="${POOLS:-}"
PORT="${PORT:-3001}"
AMARU_STARTUP_REQUIRED="${AMARU_STARTUP_REQUIRED:-false}"
AMARU_STARTUP_DIR="${AMARU_STARTUP_DIR:-/amaru-startup}"
AMARU_RELAYS="${AMARU_RELAYS:-}"

# Antithesis
ANTITHESIS_OUTPUT_DIR="${ANTITHESIS_OUTPUT_DIR:-/tmp}"

verify_environment_variables() {
    if [ -z "${POOLS}" ]; then
        echo "POOLS not defined, exiting..."
        sleep 60
        exit 1
    fi
    case "${AMARU_STARTUP_REQUIRED}" in
        true | TRUE | 1 | yes | YES)
            if [ -z "${AMARU_RELAYS}" ]; then
                echo "AMARU_RELAYS not defined, exiting..."
                sleep 60
                exit 1
            fi
            ;;
    esac
}

signal_ready() {
    if [ ! -f "${ANTITHESIS_OUTPUT_DIR}/sdk.jsonl" ]; then
        for i in $(seq 1 "${POOLS}"); do
            (
                while true; do
                    # Setup readiness must use the same tip protocol path as
                    # convergence checks; plain ping can succeed before --tip.
                    if cardano-cli ping -c1 -q -j --tip \
                        --magic 42 --host "p${i}.example" --port "${PORT}" \
                        >/dev/null 2>&1; then
                        break
                    fi
                    sleep 1
                done
            ) &
        done
        wait
        wait_for_amaru_startup
        echo '{"antithesis_setup": { "status": "complete", "details": null }}' >"${ANTITHESIS_OUTPUT_DIR}/sdk.jsonl"
    fi
}

wait_for_amaru_startup() {
    case "${AMARU_STARTUP_REQUIRED}" in
        true | TRUE | 1 | yes | YES) ;;
        *) return 0 ;;
    esac

    while true; do
        missing=0
        for relay in ${AMARU_RELAYS}; do
            if [ ! -f "${AMARU_STARTUP_DIR}/${relay}.started" ]; then
                missing=1
                break
            fi
        done

        if [ "${missing}" -eq 0 ]; then
            return 0
        fi

        sleep 1
    done
}

# Establish run order
main() {
    verify_environment_variables
    signal_ready
    while true; do
        sleep 60
    done
}

main
