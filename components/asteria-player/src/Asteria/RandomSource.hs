{- |
Module      : Asteria.RandomSource
Description : Pluggable randomness for player decisions.

Every player decision (which direction to move, when to mine,
how long to sleep) goes through a 'RandomSource'. The default
implementation uses 'System.Random' with a per-container seed
('newSystemSource'). Iteration 7 will add an Antithesis-driven
implementation that subprocesses to Python's
@antithesis.random.get_random()@ so the hypervisor controls
every game decision and can drive state-space exploration.

The interface is a record-of-functions rather than a typeclass
so the active source can be swapped at runtime by reading an
env var.
-}
module Asteria.RandomSource (
    RandomSource (..),
    newSystemSource,
    randomInRange,
) where

import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Word (Word64)
import System.Random (StdGen, mkStdGen, randomR)

{- | A random-source handle. 'getU64' returns a fresh @Word64@
on every call; thread-safety is the implementation's
responsibility.
-}
newtype RandomSource = RandomSource
    { getU64 :: IO Word64
    }

{- | A 'System.Random'-backed source seeded from the player's
identifier so each container has a deterministic-but-distinct
sequence. Drop-in fallback when the Antithesis SDK isn't
reachable.
-}
newSystemSource :: Int -> IO RandomSource
newSystemSource seed = do
    ref <- newIORef (mkStdGen seed)
    pure
        RandomSource
            { getU64 = atomicModifyIORef' ref drawWord64
            }

drawWord64 :: StdGen -> (StdGen, Word64)
drawWord64 g =
    let (w, g') = randomR (minBound, maxBound) g
     in (g', w)

{- | Draw an integer in @[lo, hi]@ inclusive. Implemented via
modulo, which biases when @hi - lo + 1@ doesn't divide @2^64@,
but the bias is below 1e-18 for practical asteria ranges
(direction deltas in @-9..9@, sleep intervals in @1..30@).
-}
randomInRange :: RandomSource -> Integer -> Integer -> IO Integer
randomInRange src lo hi = do
    w <- getU64 src
    let span_ = hi - lo + 1
    pure (lo + fromIntegral w `mod` span_)
