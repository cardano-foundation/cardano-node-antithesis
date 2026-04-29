#!/usr/bin/env bash
# entrypoint.sh — converts a TextEnvelope-shaped Cardano
# signing key to the 32-byte raw seed that
# cardano-tx-generator expects, then execs the daemon.
#
# The testnet's faucet skey is in TextEnvelope JSON
# (because the cluster's configurator emits the same
# format cardano-cli uses):
#
#     { "type": "PaymentSigningKeyShelley_ed25519",
#       "description": "...",
#       "cborHex": "5820<32-byte hex>" }
#
# The cborHex is `5820` (CBOR header for "byte string of
# length 32") followed by the raw 32 bytes hex-encoded.
# We strip the prefix, decode hex, write the 32 bytes,
# and substitute the path in the daemon's CLI args.

set -euo pipefail

ARGS=("$@")
FAUCET_IDX=

for i in "${!ARGS[@]}"; do
    if [ "${ARGS[$i]}" = "--faucet-skey-file" ]; then
        FAUCET_IDX=$((i + 1))
        break
    fi
done

if [ -n "$FAUCET_IDX" ] && [ -n "${ARGS[FAUCET_IDX]:-}" ]; then
    FAUCET_PATH="${ARGS[FAUCET_IDX]}"
    if [ -r "$FAUCET_PATH" ] \
        && head -c 1 "$FAUCET_PATH" 2>/dev/null \
        | grep -q '{'; then
        echo "[entrypoint] converting TextEnvelope" \
            "$FAUCET_PATH to raw 32-byte seed" >&2
        HEX="$(jq -r '.cborHex' "$FAUCET_PATH")"
        RAW_HEX="${HEX#5820}"
        if [ "${#RAW_HEX}" -ne 64 ]; then
            echo "[entrypoint] unexpected cborHex length:" \
                "${#RAW_HEX} (expected 64)" >&2
            exit 1
        fi
        # Use the daemon's --state-dir as scratch. /tmp
        # isn't pre-created in dockerTools.buildImage's
        # rootfs, but the rw /state mount always is.
        TMP="/state/faucet-raw.skey"
        printf '%s' "$RAW_HEX" | xxd -r -p > "$TMP"
        if [ "$(stat -c %s "$TMP")" -ne 32 ]; then
            echo "[entrypoint] decoded seed is not 32" \
                "bytes" >&2
            exit 1
        fi
        ARGS[FAUCET_IDX]="$TMP"
    fi
fi

exec /bin/cardano-tx-generator "${ARGS[@]}"
