{- |
Module      : Main
Description : Iteration-1 stub for the asteria-player long-running loop.

Emits 'sdkReachable' to the JSONL fallback, then sleeps forever so
the container stays alive for inspection. Subsequent iterations
replace this body with the actual game loop:

  1. Resolve the N2C socket path from @CARDANO_NODE_SOCKET_PATH@.
  2. Build a 'Provider' + 'Submitter' against it.
  3. Loop:
        state <- queryGameState provider
        action <- pickAction antithesisRandom state
        signed <- buildAndSign action
        Submitted _ <- submitTx submitter signed
        threadDelay (waitFor antithesisRandom)
-}
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Text qualified as T
import System.Environment (lookupEnv)

import Asteria.Sdk (sdkReachable)

main :: IO ()
main = do
    playerId <- maybe "asteria-player" id <$> lookupEnv "ASTERIA_PLAYER_ID"
    sdkReachable
        ("asteria_player_started_" <> T.pack playerId)
        Nothing
    forever (threadDelay 60_000_000) -- 60s
