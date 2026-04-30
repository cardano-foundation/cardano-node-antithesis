{- |
Module      : Asteria.Crypto
Description : Bridge between cardano-crypto-class hashes and PlutusTx
              builtin bytestrings.

Tiny helper so the bootstrap can express
@spacetime_hash@ as a @BuiltinByteString@ inside an
@AsteriaDatum@ without leaking hash-class internals.
-}
module Asteria.Crypto (
    CardanoHash,
    hashToBuiltinByteString,
) where

import Cardano.Crypto.Hash.Class (Hash, hashToBytes)
import PlutusTx.Builtins.Internal (BuiltinByteString (..))

{- | Re-export so callers don't need to import the cardano-crypto
type just to write the constraint.
-}
type CardanoHash h a = Hash h a

hashToBuiltinByteString :: Hash h a -> BuiltinByteString
hashToBuiltinByteString = BuiltinByteString . hashToBytes
