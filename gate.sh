#!/usr/bin/env bash
# Mechanical gate for the feat/consumer-convergence-property PR (#180).
# The tracer-sidecar test suite is the RED->GREEN harness for the property.
set -euo pipefail

git diff --check

# tracer-sidecar property tests (hspec + golden). Prefer the hermetic nix build
# of the test derivation; fall back to cabal in the component dir.
if command -v nix >/dev/null 2>&1 && [ -f components/tracer-sidecar/flake.nix ]; then
  ( cd components/tracer-sidecar && nix build --quiet '.#tracer-sidecar-tests' )
else
  ( cd components/tracer-sidecar && cabal test --test-show-details=streaming )
fi

# Compose still validates with the tracer-sidecar env + image bump (slice 2).
INTERNAL_NETWORK=false docker compose \
  -f testnets/cardano_amaru/docker-compose.yaml config >/dev/null

# master must NOT gain the consumer convergence env (would redden master runs).
if grep -nE 'AMARU_CONSUMER_HOST' testnets/cardano_node_master/docker-compose.yaml >/dev/null 2>&1; then
  echo "gate: cardano_node_master must not set AMARU_CONSUMER_HOST" >&2
  exit 1
fi

echo "gate: OK"
