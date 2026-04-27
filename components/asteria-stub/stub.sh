#!/usr/bin/env bash
# stub.sh — long-running placeholder for the asteria-stub container.
#
# Antithesis schedules composer commands via `docker exec` against this
# container; the container itself just needs to stay alive so the test
# composer has a host to invoke parallel_driver / eventually_ / finally_
# scripts inside. No real workload runs here.

set -u
exec sleep infinity
