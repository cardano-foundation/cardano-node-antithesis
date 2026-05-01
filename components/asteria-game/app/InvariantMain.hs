{- |
Module      : Main
Description : Invariant snapshot for anytime_/finally_ drivers.

Reads the @ASTERIA_INVARIANT@ env var to pick which game-state
property to assert against the current chain state, then emits
exactly one SDK assertion describing whether the property holds:

  * @admin_singleton@ — 'sdkAlways' that exactly one
    @asteriaAdmin@ NFT sits at the asteria spend address. The
    bootstrap mints the NFT once and locks it; nothing in the
    designed game flow ever burns or duplicates it, so any
    deviation is a real bug.

  * @consistency@ — 'sdkSometimes' that the asteria UTxO's
    @ship_counter@ equals the number of @SHIP*@ tokens currently
    locked at the spacetime spend address. After @N@ spawns and
    @0@ quits / mines the equality holds; subsequent quits or
    mines burn ships and break the equality, so this is a
    sometimes-property — Antithesis should observe it true at
    least once per timeline.

The driver scripts decide cadence (anytime vs finally); this
binary is one-shot per invocation.
-}
module Main (main) where

import Control.Exception (SomeException, try)
import Data.Aeson (object, (.=))
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Short qualified as SBS
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Lens.Micro ((^.))
import System.Environment (lookupEnv)

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Api.Tx (getPlutusData)
import Cardano.Ledger.Api.Tx.Out (TxOut, datumTxOutL, valueTxOutL)
import Cardano.Ledger.BaseTypes (Network (Testnet))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (hashScript)
import Cardano.Ledger.Credential (
    Credential (ScriptHashObj),
    StakeReference (StakeRefNull),
 )
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Mary.Value (
    AssetName (..),
    MaryValue (..),
    MultiAsset (..),
    PolicyID (..),
 )
import Cardano.Ledger.Plutus.Data (Datum (..), binaryDataToData)
import Cardano.Node.Client.Provider (Provider (..))
import PlutusTx.Builtins.Internal (BuiltinData (..))
import PlutusTx.IsData.Class (fromBuiltinData)

import Asteria.Datums (AsteriaDatum (..))
import Asteria.Provider (N2CSettings, settingsFromEnv, withN2C)
import Asteria.Sdk (sdkAlways, sdkReachable, sdkSometimes, sdkUnreachable)
import Asteria.Validators (
    adminMintScript,
    asteriaScript,
    spacetimeScript,
 )

main :: IO ()
main = do
    invariant <-
        T.pack
            . fromMaybe "admin_singleton"
            <$> lookupEnv "ASTERIA_INVARIANT"
    sdkReachable
        ("asteria_invariant_started_" <> invariant)
        Nothing
    settings <- settingsFromEnv
    result <- try (run invariant settings)
    case result of
        Left (e :: SomeException) ->
            sdkUnreachable
                ("asteria_invariant_errored_" <> invariant)
                (Just $ object ["error" .= T.pack (show e)])
        Right () -> pure ()
    sdkReachable ("asteria_invariant_completed_" <> invariant) Nothing

run :: Text -> N2CSettings -> IO ()
run invariant settings = withN2C settings $ \provider _submitter ->
    case invariant of
        "admin_singleton" -> checkAdminSingleton provider
        "consistency" -> checkConsistency provider
        other ->
            sdkUnreachable
                "asteria_invariant_unknown"
                (Just $ object ["invariant" .= other])

{- | Invariant: exactly one @asteriaAdmin@ NFT exists at the
asteria spend address (across all UTxOs).
-}
checkAdminSingleton :: Provider IO -> IO ()
checkAdminSingleton provider = do
    let addr = scriptAddr (hashScript asteriaScript)
        adminPolicy = PolicyID (hashScript adminMintScript)
        adminName = AssetName "asteriaAdmin"
    utxos <- queryUTxOs provider addr
    let total = sum (map (countAsset adminPolicy adminName . snd) utxos)
        ok = total == 1
    sdkAlways
        ok
        "asteria_admin_singleton"
        ( Just $
            object
                [ "asteria_admin_count" .= total
                , "asteria_addr" .= T.pack (show addr)
                , "utxos_at_addr" .= length utxos
                ]
        )

{- | Invariant: @ship_counter@ from the asteria UTxO's datum
equals the number of @SHIP*@ tokens at the spacetime spend
address. True after pure-spawn flows; false after quit/mine.
-}
checkConsistency :: Provider IO -> IO ()
checkConsistency provider = do
    let asteriaAddr = scriptAddr (hashScript asteriaScript)
        shipAddr = scriptAddr (hashScript spacetimeScript)
        shipyardPolicy = PolicyID (hashScript spacetimeScript)
    asteriaUtxos <- queryUTxOs provider asteriaAddr
    shipUtxos <- queryUTxOs provider shipAddr
    let counter = case asteriaUtxos of
            ((_, out) : _) -> case decodeAsteria out of
                Just d -> Just (adShipCounter d)
                Nothing -> Nothing
            [] -> Nothing
        shipCount = sum (map (countShipTokens shipyardPolicy . snd) shipUtxos)
    case counter of
        Just c ->
            sdkSometimes
                (c == fromIntegral shipCount)
                "asteria_state_consistent"
                ( Just $
                    object
                        [ "ship_counter" .= c
                        , "ship_token_count" .= shipCount
                        ]
                )
        Nothing ->
            sdkUnreachable
                "asteria_state_undecodable"
                (Just $ object ["asteria_utxos" .= length asteriaUtxos])

scriptAddr :: ScriptHash -> Addr
scriptAddr h = Addr Testnet (ScriptHashObj h) StakeRefNull

countAsset :: PolicyID -> AssetName -> TxOut ConwayEra -> Integer
countAsset pid an out =
    case out ^. valueTxOutL of
        MaryValue _ (MultiAsset assets) ->
            case Map.lookup pid assets of
                Just inner -> Map.findWithDefault 0 an inner
                Nothing -> 0

{- | Sum of token quantities under @shipyardPolicy@ whose name
begins with the @"SHIP"@ prefix (excludes PILOT NFTs).
-}
countShipTokens :: PolicyID -> TxOut ConwayEra -> Int
countShipTokens pid out =
    case out ^. valueTxOutL of
        MaryValue _ (MultiAsset assets) ->
            case Map.lookup pid assets of
                Just inner ->
                    sum
                        [ fromIntegral q
                        | (AssetName sbs, q) <- Map.toList inner
                        , let bs = SBS.fromShort sbs
                        , BS8.isPrefixOf "SHIP" bs
                        , q > 0
                        ]
                Nothing -> 0

decodeAsteria :: TxOut ConwayEra -> Maybe AsteriaDatum
decodeAsteria out = case out ^. datumTxOutL of
    Datum bd ->
        fromBuiltinData
            (BuiltinData (getPlutusData (binaryDataToData bd)))
    _ -> Nothing
