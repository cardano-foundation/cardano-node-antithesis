#!/bin/sh
# N2C service harness for Antithesis.
#
# Wraps an N2C service (ogmios, kupo) and:
# - Waits for the node socket before starting
# - Converts SIGTERM to SIGINT for Haskell processes
#   (GHC handles SIGINT gracefully but not SIGTERM by default,
#    see: https://github.com/IntersectMBO/cardano-node/issues/2267)
# - Logs non-zero exits with signal context
#
# Usage: harness.sh <socket-path> <command> [args...]

SOCKET="$1"
shift

echo "[harness] waiting for socket: $SOCKET"
while [ ! -S "$SOCKET" ]; do
    sleep 2
done
echo "[harness] socket ready, starting: $*"

# Run child in background so we can forward signals
"$@" &
CHILD=$!

# Convert SIGTERM to SIGINT for GHC (Haskell) processes
# GHC only handles SIGINT (UserInterrupt), SIGTERM kills without cleanup
SIGNALED=false
trap 'SIGNALED=true; echo "[harness] SIGTERM received, sending SIGINT to child ($CHILD)"; kill -INT $CHILD 2>/dev/null' TERM
trap 'SIGNALED=true; echo "[harness] SIGINT received, forwarding to child ($CHILD)"; kill -INT $CHILD 2>/dev/null' INT

# Wait for child to exit
wait $CHILD
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    echo "[harness] clean exit (0)"
    exit 0
fi

if [ "$SIGNALED" = true ]; then
    echo "[harness] child exited ($EXIT_CODE) after signal"
fi

exit $EXIT_CODE
