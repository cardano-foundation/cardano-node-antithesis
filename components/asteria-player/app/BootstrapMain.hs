{- |
Module      : Main
Description : Iteration-1 stub for the asteria game bootstrap.

Emits 'sdkReachable' so the SDK fallback file shows the bootstrap
container ran, then exits 0. Subsequent iterations replace this with
the actual asteria bootstrap tx:

  1. Compile + parameter-apply the 4 Aiken validators.
  2. Submit a tx that:
       - mints the asteria admin NFT,
       - locks the asteria UTxO at the asteria spend address with
         inline @AsteriaDatum {ship_counter=0, shipyard_policy=…}@,
       - deploys all four validators as inline reference scripts,
       - mints the initial pellet UTxOs at known coordinates.

When this exits 0 the docker-compose @depends_on@ on
@service_completed_successfully@ unblocks the player containers.
-}
module Main (main) where

import Asteria.Sdk (sdkReachable)

main :: IO ()
main = sdkReachable "asteria_bootstrap_completed" Nothing
