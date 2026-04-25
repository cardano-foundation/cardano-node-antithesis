{- |
Module      : Main
Description : Iteration-2 — query protocol params via N2C in a loop.

Connects to the cardano-node N2C socket at
@$CARDANO_NODE_SOCKET_PATH@ using the queue / async pattern from
cardano-node-clients's @withDevnet@. Once connected, queries protocol
parameters in a loop (every 5 seconds) and emits an
@asteria_player_pp_query_<id>@ 'sdk_sometimes' event so the antithesis
hypervisor can confirm the player is genuinely talking to a node.

Subsequent iterations replace the loop body with actual game state
queries + tx submissions.
-}
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import System.Environment (lookupEnv)

import Asteria.Provider (settingsFromEnv, withN2C)
import Asteria.Sdk (sdkReachable, sdkSometimes, sdkUnreachable)
import Asteria.Validators (asteriaScript, deployScript, pelletScript, spacetimeScript)
import Cardano.Ledger.Core (hashScript)
import Cardano.Node.Client.Provider (Provider (..))

main :: IO ()
main = do
    playerIdStr <-
        maybe "unknown" id <$> lookupEnv "ASTERIA_PLAYER_ID"
    let playerId = T.pack playerIdStr
    sdkReachable
        ("asteria_player_started_" <> playerId)
        Nothing
    -- Touch each validator so the blueprint-load machinery is
    -- exercised at startup (not lazily on first game action).
    -- Iteration 4 will swap these unapplied scripts for
    -- parameter-applied versions and start using their hashes
    -- as part of bootstrap.
    let validatorHashes =
            [ T.pack (show (hashScript asteriaScript))
            , T.pack (show (hashScript spacetimeScript))
            , T.pack (show (hashScript pelletScript))
            , T.pack (show (hashScript deployScript))
            ]
    sdkReachable
        ("asteria_player_validators_loaded_" <> playerId)
        ( Just $
            object ["hashes" .= validatorHashes]
        )
    settings <- settingsFromEnv
    withN2C settings $ \provider _submitter -> do
        sdkReachable
            ("asteria_player_n2c_connected_" <> playerId)
            Nothing
        forever (loop provider playerId)
  where
    loop provider playerId = do
        result <- try (queryProtocolParams provider)
        case result of
            Left (e :: SomeException) ->
                sdkUnreachable
                    ("asteria_player_pp_query_failed_" <> playerId)
                    ( Just $
                        object ["error" .= T.pack (show e)]
                    )
            Right _pp ->
                sdkSometimes
                    True
                    ("asteria_player_pp_query_" <> playerId)
                    Nothing
        threadDelay 5_000_000
