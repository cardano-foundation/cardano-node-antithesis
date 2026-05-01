{- |
Module      : Main
Description : Iteration-6c player single-pass — attempt spawnShip once.

Each invocation does one pass:

  1. Observe the asteria UTxO at the asteria spend address.
  2. If this player is the spawn-eligible player (currently
     @ASTERIA_PLAYER_ID=1@) attempt 'spawnShipProgram': build,
     sign, submit. Emit @asteria_player_ship_spawn_attempted_<id>@
     on the try, @asteria_player_ship_spawned_<id>@ on success,
     @asteria_player_ship_spawn_failed_<id>@ on rejection.
  3. Exit 0.

The Antithesis composer re-fires the parallel driver on its own
schedule, so a forever loop here would block exclusive scheduling
for downstream serial drivers. Spawn idempotence comes from chain
state — once the asteria UTxO has been consumed-and-replaced with
a higher @ship_counter@, the next attempt's tx will be rejected as
expected.
-}
module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Aeson (object, (.=))
import Data.Foldable (toList)
import Data.Hashable (hash)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import System.Environment (lookupEnv)

import Cardano.Crypto.DSIGN (
    Ed25519DSIGN,
    SignKeyDSIGN,
    deriveVerKeyDSIGN,
    signedDSIGN,
 )
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Api.Tx (bodyTxL, getPlutusData, txIdTx, witsTxL)
import Cardano.Ledger.Api.Tx.Body (outputsTxBodyL)
import Cardano.Ledger.Api.Tx.Out (TxOut, datumTxOutL)
import Cardano.Ledger.Api.Tx.Wits (addrTxWitsL)
import Cardano.Ledger.BaseTypes (Network (Testnet), StrictMaybe (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (hashScript)
import Cardano.Ledger.Credential (
    Credential (ScriptHashObj),
    StakeReference (StakeRefNull),
 )
import Cardano.Ledger.Hashes (ScriptHash, extractHash)
import Cardano.Ledger.Keys (
    KeyRole (Witness),
    VKey (..),
    WitVKey (..),
    asWitness,
 )
import Cardano.Ledger.Mary.Value (AssetName (..), PolicyID (..))
import Cardano.Ledger.Plutus.Data (Datum (..), binaryDataToData)
import Cardano.Ledger.TxIn (TxId (..), TxIn)
import Cardano.Node.Client.Ledger (ConwayTx)
import Cardano.Node.Client.Provider (Provider (..))
import Cardano.Node.Client.Submitter (
    SubmitResult (..),
    Submitter (..),
 )
import Cardano.Node.Client.TxBuild (InterpretIO (..), build)
import Cardano.Slotting.Slot (SlotNo (..))
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Short qualified as SBS
import Data.Set qualified as Set
import Data.Time.Clock.POSIX (getPOSIXTime)
import Lens.Micro ((%~), (&), (^.))
import PlutusTx.Builtins.Internal (BuiltinData (..))
import PlutusTx.IsData.Class (fromBuiltinData)

import Asteria.Datums (AsteriaDatum (..))
import Asteria.Game (
    SpawnShipParams (..),
    spawnShipProgram,
 )
import Asteria.Provider (settingsFromEnv, withN2C)
import Asteria.RandomSource (
    RandomSource,
    newSystemSource,
    randomInRange,
 )
import Asteria.Sdk (sdkReachable, sdkSometimes, sdkUnreachable)
import Asteria.Validators (
    adminMintScript,
    asteriaScript,
    pelletScript,
    spacetimeScript,
 )
import Asteria.Wallet (
    WalletKey (..),
    genesisKeyPath,
    pickWalletUtxo,
    readWalletKey,
    walletAddr,
 )

main :: IO ()
main = do
    playerIdStr <-
        fromMaybe "unknown" <$> lookupEnv "ASTERIA_PLAYER_ID"
    let playerId = T.pack playerIdStr
    sdkReachable
        ("asteria_player_started_" <> playerId)
        Nothing
    rng <- newSystemSource (hash playerIdStr)
    settings <- settingsFromEnv
    walletKey <- readWalletKey genesisKeyPath
    withN2C settings $ \provider submitter -> do
        sdkReachable
            ("asteria_player_n2c_connected_" <> playerId)
            Nothing
        result <- try (observeAndAct provider submitter walletKey rng playerId)
        case result of
            Left (e :: SomeException) ->
                sdkUnreachable
                    ("asteria_player_pass_errored_" <> playerId)
                    (Just $ object ["error" .= T.pack (show e)])
            Right () -> pure ()
    sdkReachable ("asteria_player_pass_completed_" <> playerId) Nothing

observeAndAct ::
    Provider IO ->
    Submitter IO ->
    WalletKey ->
    RandomSource ->
    Text ->
    IO ()
observeAndAct provider submitter wk rng playerId = do
    let asteriaAddr = scriptAddr (hashScript asteriaScript)
    outs <- queryUTxOs provider asteriaAddr
    case outs of
        [] -> do
            sdkSometimes
                False
                ("asteria_player_asteria_observed_" <> playerId)
                Nothing
        ((aIn, aOut) : _) -> do
            sdkSometimes
                True
                ("asteria_player_asteria_observed_" <> playerId)
                Nothing
            case decodeAsteria aOut of
                Nothing ->
                    sdkUnreachable
                        ("asteria_player_datum_decode_failed_" <> playerId)
                        Nothing
                Just datum -> do
                    sdkReachable
                        ("asteria_player_ship_counter_" <> playerId)
                        (Just $ object ["ship_counter" .= adShipCounter datum])
                    -- Plan a (delta_x, delta_y) every iteration.
                    dx <- randomInRange rng (-5) 5
                    dy <- randomInRange rng (-5) 5
                    sdkSometimes
                        True
                        ("asteria_player_move_planned_" <> playerId)
                        (Just $ object ["delta_x" .= dx, "delta_y" .= dy])
                    -- Only player 1 attempts to spawn — avoids
                    -- multi-player races at this iteration. The
                    -- chain rejects double-spawn naturally because
                    -- the asteria UTxO is consumed-and-replaced.
                    when (playerId == "1") $
                        attemptSpawn
                            provider
                            submitter
                            wk
                            (aIn, aOut)
                            datum
                            playerId

attemptSpawn ::
    Provider IO ->
    Submitter IO ->
    WalletKey ->
    (TxIn, TxOut ConwayEra) ->
    AsteriaDatum ->
    Text ->
    IO ()
attemptSpawn provider submitter wk (aIn, aOut) datum playerId = do
    sdkReachable
        ("asteria_player_ship_spawn_attempted_" <> playerId)
        Nothing
    pp <- queryProtocolParams provider
    -- Compute a fresh validity-range upper bound from the
    -- current wallclock so the tx isn't rejected as
    -- @OutsideValidityIntervalUTxO@ on submission.
    nowMs <- floor . (* 1000) <$> getPOSIXTime
    SlotNo nowSlot <- posixMsToSlot provider nowMs
    let validToSlot = SlotNo (nowSlot + 60)
    fundingSeed@(fundingIn, _) <- pickWalletUtxo provider wk
    let asteriaAddr = scriptAddr (hashScript asteriaScript)
        shipAddr = scriptAddr (hashScript spacetimeScript)
        adminPolicy = PolicyID (hashScript adminMintScript)
        shipyardPolicy = PolicyID (hashScript spacetimeScript)
        fuelPolicy = PolicyID (hashScript pelletScript)
        params =
            SpawnShipParams
                { sspAsteriaIn = aIn
                , sspAsteriaOut = aOut
                , sspAsteriaAddr = asteriaAddr
                , sspShipAddr = shipAddr
                , sspAsteriaDatum = datum
                , sspAdminPolicy = adminPolicy
                , sspAdminName = AssetName (SBS.toShort (BS8.pack "asteriaAdmin"))
                , sspShipyardPolicy = shipyardPolicy
                , sspFuelPolicy = fuelPolicy
                , sspAsteriaScript = asteriaScript
                , sspSpacetimeScript = spacetimeScript
                , sspPelletScript = pelletScript
                , sspFundingIn = fundingIn
                , sspValidTo = validToSlot
                , sspPilotAddr = walletAddr wk
                }
        eval tx =
            fmap (Map.map (either (Left . show) Right)) (evaluateTx provider tx)
        interpret :: InterpretIO NoQ
        interpret = InterpretIO $ \case {}
    built <-
        build
            pp
            interpret
            eval
            [(aIn, aOut), fundingSeed]
            []
            (walletAddr wk)
            (spawnShipProgram params)
    case built of
        Left err ->
            sdkUnreachable
                ("asteria_player_ship_spawn_failed_" <> playerId)
                (Just $ object ["stage" .= ("build" :: Text), "error" .= T.pack (show err)])
        Right tx -> do
            let signed = addKeyWitness (wkSignKey wk) tx
                outs = toList (signed ^. bodyTxL . outputsTxBodyL)
            sdkReachable
                ("asteria_player_ship_spawn_built_" <> playerId)
                (Just $ object ["outputs" .= length outs])
            r <- submitTx submitter signed
            case r of
                Submitted _ ->
                    sdkSometimes
                        True
                        ("asteria_player_ship_spawned_" <> playerId)
                        Nothing
                Rejected reason ->
                    sdkUnreachable
                        ("asteria_player_ship_spawn_failed_" <> playerId)
                        ( Just $
                            object
                                [ "stage" .= ("submit" :: Text)
                                , "error" .= T.pack (show reason)
                                ]
                        )

-- | Phantom query GADT — player has no @ctx@ queries.
data NoQ a

scriptAddr :: ScriptHash -> Addr
scriptAddr h = Addr Testnet (ScriptHashObj h) StakeRefNull

decodeAsteria :: TxOut ConwayEra -> Maybe AsteriaDatum
decodeAsteria out = case out ^. datumTxOutL of
    Datum bd ->
        fromBuiltinData
            (BuiltinData (getPlutusData (binaryDataToData bd)))
    _ -> Nothing

addKeyWitness ::
    SignKeyDSIGN Ed25519DSIGN -> ConwayTx -> ConwayTx
addKeyWitness sk tx =
    tx & witsTxL . addrTxWitsL %~ Set.union (Set.singleton w)
  where
    TxId h = txIdTx tx
    vk = VKey (deriveVerKeyDSIGN sk)
    w = WitVKey (asWitness vk) (signedDSIGN () (extractHash h) sk)
