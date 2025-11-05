#!/usr/bin/env bash

set -o pipefail

SHELL="/bin/bash"
PATH="/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin"

SLEEP="${SLEEP:-60}"

sleep_for_awhile() {
    sleep ${SLEEP}
}

# Establish run order
main() {
    sleep_for_awhile
}

main
