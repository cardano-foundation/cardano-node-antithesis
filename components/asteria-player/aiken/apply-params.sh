#!/usr/bin/env bash
# apply-params.sh — chains `aiken blueprint apply` calls to bake
# the asteria game parameters into the four validators.
#
# Output: plutus-applied.json with the parameter-applied scripts.
# Re-run any time parameters change. The CBOR values below were
# computed with Python's cbor2 against Plutus Data shapes:
#
#   admin_token       = AssetClass { policy: 00..00 (28 zero bytes)
#                                  , name:   "asteriaAdmin" }
#                     = Constr 0 [B 00..00, B "asteriaAdmin"]
#   max_speed         = Speed { distance: 1, time: 30000 }
#                     = Constr 0 [I 1, I 30000]
#   integers are direct CBOR encodings.
#
# Cross-validator hash dependencies (handled by the chain order):
#   asteria   needs pellet's hash
#   spacetime needs pellet's + asteria's hashes
#
# Usage: ./apply-params.sh  (run from this directory, after
#        a fresh `aiken build` has produced plutus.json)

set -euo pipefail
cd "$(dirname "$0")"

# Parameter CBOR (computed once with Python cbor2 — see header).
ADMIN_TOKEN_CBOR=d87982581c000000000000000000000000000000000000000000000000000000004c6173746572696141646d696e
SHIP_MINT_LOVELACE_FEE_CBOR=1a002dc6c0   # 3_000_000
MAX_ASTERIA_MINING_CBOR=1832             # 50
MIN_ASTERIA_DISTANCE_CBOR=1832           # 50
INITIAL_FUEL_CBOR=1864                   # 100
MAX_SPEED_CBOR=d8798201197530            # Speed { distance=1, time=30000 }
MAX_SHIP_FUEL_CBOR=1864                  # 100
FUEL_PER_STEP_CBOR=05                    # 5

aiken build

# 1. Apply admin_token to pellet.
aiken blueprint apply \
    -i plutus.json -o p1.json \
    -m pellet -v pellet \
    "$ADMIN_TOKEN_CBOR"

# 2. Apply admin_token to deploy.
aiken blueprint apply \
    -i p1.json -o p2.json \
    -m deploy -v deploy \
    "$ADMIN_TOKEN_CBOR"

# 3. Capture pellet's parameter-applied hash for the asteria /
#    spacetime parameters that reference it. Must be re-encoded as
#    a CBOR ByteString (28 bytes = 0x1C → header 581C).
PELLET_HASH=$(jq -r '.validators[] | select(.title=="pellet.pellet.spend") | .hash' p2.json)
PELLET_HASH_CBOR="581c$PELLET_HASH"

# 4. Apply asteria's six parameters in declaration order:
#      pellet_address, admin_token,
#      ship_mint_lovelace_fee, max_asteria_mining,
#      min_asteria_distance, initial_fuel
aiken blueprint apply -i p2.json -o p3a.json -m asteria -v asteria "$PELLET_HASH_CBOR"
aiken blueprint apply -i p3a.json -o p3b.json -m asteria -v asteria "$ADMIN_TOKEN_CBOR"
aiken blueprint apply -i p3b.json -o p3c.json -m asteria -v asteria "$SHIP_MINT_LOVELACE_FEE_CBOR"
aiken blueprint apply -i p3c.json -o p3d.json -m asteria -v asteria "$MAX_ASTERIA_MINING_CBOR"
aiken blueprint apply -i p3d.json -o p3e.json -m asteria -v asteria "$MIN_ASTERIA_DISTANCE_CBOR"
aiken blueprint apply -i p3e.json -o p3.json  -m asteria -v asteria "$INITIAL_FUEL_CBOR"

# 5. Capture asteria's hash for spacetime.
ASTERIA_HASH=$(jq -r '.validators[] | select(.title=="asteria.asteria.spend") | .hash' p3.json)
ASTERIA_HASH_CBOR="581c$ASTERIA_HASH"

# 6. Apply spacetime's six parameters in declaration order:
#      pellet_validator_address, asteria_validator_address,
#      admin_token, max_speed, max_ship_fuel, fuel_per_step
aiken blueprint apply -i p3.json  -o p4a.json -m spacetime -v spacetime "$PELLET_HASH_CBOR"
aiken blueprint apply -i p4a.json -o p4b.json -m spacetime -v spacetime "$ASTERIA_HASH_CBOR"
aiken blueprint apply -i p4b.json -o p4c.json -m spacetime -v spacetime "$ADMIN_TOKEN_CBOR"
aiken blueprint apply -i p4c.json -o p4d.json -m spacetime -v spacetime "$MAX_SPEED_CBOR"
aiken blueprint apply -i p4d.json -o p4e.json -m spacetime -v spacetime "$MAX_SHIP_FUEL_CBOR"
aiken blueprint apply -i p4e.json -o plutus-applied.json \
    -m spacetime -v spacetime "$FUEL_PER_STEP_CBOR"

# Cleanup intermediate files.
rm -f p1.json p2.json p3a.json p3b.json p3c.json p3d.json p3e.json p3.json \
      p4a.json p4b.json p4c.json p4d.json p4e.json

echo "applied parameters → plutus-applied.json"
echo "  pellet:    $PELLET_HASH"
echo "  asteria:   $ASTERIA_HASH"
jq -r '.validators[] | select(.title|endswith(".spend")) | "  \(.title)\t\(.hash)"' plutus-applied.json
