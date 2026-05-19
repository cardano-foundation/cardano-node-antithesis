#!/usr/bin/env bash
# amaru-prober entrypoint: wait for every relay's startup marker to
# appear in /amaru-startup/, then idle so the Antithesis composer can
# `docker exec` the dispatch scripts under /opt/antithesis/test/v1/amaru/.

set -o pipefail

AMARU_STARTUP_DIR="${AMARU_STARTUP_DIR:-/amaru-startup}"
AMARU_RELAYS="${AMARU_RELAYS:-amaru-relay-1 amaru-relay-2}"
ANTITHESIS_OUTPUT_DIR="${ANTITHESIS_OUTPUT_DIR:-/tmp}"

mkdir -p "$ANTITHESIS_OUTPUT_DIR"

echo "amaru-prober: waiting for relay markers in $AMARU_STARTUP_DIR (relays: $AMARU_RELAYS)"
for relay in $AMARU_RELAYS; do
    while [ ! -f "$AMARU_STARTUP_DIR/${relay}.started" ]; do
        sleep 1
    done
    echo "amaru-prober: observed ${relay}.started"
done

if [ ! -f "$ANTITHESIS_OUTPUT_DIR/sdk.jsonl" ]; then
    : > "$ANTITHESIS_OUTPUT_DIR/sdk.jsonl"
fi
echo '{"antithesis_setup": {"status": "complete", "details": {"component": "amaru-prober"}}}' \
    >> "$ANTITHESIS_OUTPUT_DIR/sdk.jsonl"

echo "amaru-prober: all relays started; idling"
while true; do
    sleep 60
done
