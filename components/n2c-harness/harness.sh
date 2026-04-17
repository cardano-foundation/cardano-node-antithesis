#!/bin/sh
# N2C service harness for Antithesis.
#
# Wraps an N2C service (ogmios, kupo) and:
# - Waits for the node socket before starting
# - Forwards signals to the child process
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

# Forward SIGTERM and SIGINT to child
SIGNALED=false
trap 'SIGNALED=true; echo "[harness] received SIGTERM, forwarding to child ($CHILD)"; kill -TERM $CHILD 2>/dev/null' TERM
trap 'SIGNALED=true; echo "[harness] received SIGINT, forwarding to child ($CHILD)"; kill -INT $CHILD 2>/dev/null' INT

# Wait for child to exit
wait $CHILD
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    echo "[harness] clean exit (0)"
    exit 0
fi

if [ "$SIGNALED" = true ]; then
    echo "[harness] child exited ($EXIT_CODE) after signal — service does not handle graceful shutdown"
fi

exit $EXIT_CODE
