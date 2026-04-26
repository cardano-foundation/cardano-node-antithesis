{- |
Module      : Main
Description : Iteration-6 player loop — discover asteria, plan moves.

Once the asteria UTxO is on-chain (created by @asteria-bootstrap@),
each player container loops:

  1. Query @asteria.spend@ for the asteria UTxO; emit
     @asteria_player_asteria_observed_<id>@ when it appears.
  2. Decode the inline @AsteriaDatum@ and emit
     @asteria_player_ship_counter_<id>@ with the current
     @ship_counter@ in the details body.
  3. Pick a random move @(delta_x, delta_y)@ in @[-5, 5]@ via the
     injectable 'RandomSource' (System.Random fallback in this
     iteration; iteration 7 wires Antithesis's
     @random.get_random()@). Emit
     @asteria_player_move_planned_<id>@ with the deltas.
  4. Sleep a random interval in @[1, 5]@ seconds.

Iteration 6b will replace the "plan + emit" step with actually
building and submitting the @mintShip@ tx, then looping
@moveShip@ once a ship is owned. The DSL plumbing is already in
place from @BootstrapMain.hs@ — this iteration just stops short
of the spend so we can ship the discovery loop and Antithesis can
already see ship-counter dynamics from observation alone.
-}
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Data.Aeson (object, (.=))
import Data.Hashable (hash)
import Data.Text (Text)
import Data.Text qualified as T
import Lens.Micro ((^.))
import System.Environment (lookupEnv)

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Api.Tx (getPlutusData)
import Cardano.Ledger.Api.Tx.Out (TxOut, datumTxOutL)
import Cardano.Ledger.BaseTypes (Network (Testnet), StrictMaybe (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (hashScript)
import Cardano.Ledger.Credential (
    Credential (ScriptHashObj),
    StakeReference (StakeRefNull),
 )
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Plutus.Data (Datum (..), binaryDataToData)
import Cardano.Node.Client.Provider (Provider (..))
import PlutusTx.Builtins.Internal (BuiltinData (..))
import PlutusTx.IsData.Class (fromBuiltinData)

import Asteria.Datums (AsteriaDatum (..))
import Asteria.Provider (settingsFromEnv, withN2C)
import Asteria.RandomSource (
    RandomSource,
    newSystemSource,
    randomInRange,
 )
import Asteria.Sdk (sdkReachable, sdkSometimes, sdkUnreachable)
import Asteria.Validators (asteriaScript)

main :: IO ()
main = do
    playerIdStr <-
        maybe "unknown" id <$> lookupEnv "ASTERIA_PLAYER_ID"
    let playerId = T.pack playerIdStr
    sdkReachable
        ("asteria_player_started_" <> playerId)
        Nothing
    -- Seed by hashing the player id so each replica picks a
    -- distinct deterministic stream until iteration 7 swaps in
    -- the Antithesis source.
    rng <- newSystemSource (hash playerIdStr)
    settings <- settingsFromEnv
    withN2C settings $ \provider _submitter -> do
        sdkReachable
            ("asteria_player_n2c_connected_" <> playerId)
            Nothing
        forever (loop provider rng playerId)

loop :: Provider IO -> RandomSource -> Text -> IO ()
loop provider rng playerId = do
    result <- try (observeAndPlan provider rng playerId)
    case result of
        Left (e :: SomeException) ->
            sdkUnreachable
                ("asteria_player_loop_errored_" <> playerId)
                (Just $ object ["error" .= T.pack (show e)])
        Right () -> pure ()
    sleepSecs <- randomInRange rng 1 5
    threadDelay (fromIntegral sleepSecs * 1_000_000)

observeAndPlan :: Provider IO -> RandomSource -> Text -> IO ()
observeAndPlan provider rng playerId = do
    let asteriaAddr = scriptAddr (hashScript asteriaScript)
    outs <- queryUTxOs provider asteriaAddr
    case outs of
        [] -> do
            sdkSometimes
                False
                ("asteria_player_asteria_observed_" <> playerId)
                Nothing
        ((_, out) : _) -> do
            sdkSometimes
                True
                ("asteria_player_asteria_observed_" <> playerId)
                Nothing
            case decodeAsteria out of
                Nothing ->
                    sdkUnreachable
                        ("asteria_player_datum_decode_failed_" <> playerId)
                        Nothing
                Just AsteriaDatum{adShipCounter = c} ->
                    sdkReachable
                        ("asteria_player_ship_counter_" <> playerId)
                        (Just $ object ["ship_counter" .= c])
            dx <- randomInRange rng (-5) 5
            dy <- randomInRange rng (-5) 5
            sdkSometimes
                True
                ("asteria_player_move_planned_" <> playerId)
                ( Just $
                    object ["delta_x" .= dx, "delta_y" .= dy]
                )

scriptAddr :: ScriptHash -> Addr
scriptAddr h = Addr Testnet (ScriptHashObj h) StakeRefNull

decodeAsteria :: TxOut ConwayEra -> Maybe AsteriaDatum
decodeAsteria out = case out ^. datumTxOutL of
    Datum bd ->
        fromBuiltinData
            (BuiltinData (getPlutusData (binaryDataToData bd)))
    _ -> Nothing
