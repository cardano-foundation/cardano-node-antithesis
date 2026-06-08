#!/usr/bin/env bash
# Entrypoint for the gov-cli container: a do-nothing supervisor loop.
#
# The container holds cardano-cli plus the composer driver scripts under
# /opt/antithesis/test/v1/governance/. Antithesis `docker exec`s those
# scripts per tick; the container process itself just stays alive so
# `restart: always` has something to supervise. Idempotent: if
# Antithesis kills the container, a fresh one comes back with no lost
# state (all shared state lives on the gov-data volume).

while true; do
    echo "gov-cli idle"
    sleep 60
done
