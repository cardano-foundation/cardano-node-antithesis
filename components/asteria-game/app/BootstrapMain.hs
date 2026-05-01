{- |
Module      : Main
Description : Iteration-5b bootstrap — create the asteria UTxO.

Uses the genesis wallet from 'Asteria.Wallet' and the
parameter-applied validators from 'Asteria.Validators' to:

  1. Mint exactly one @"asteriaAdmin"@ token via the always-true
     @admin_mint@ policy (whose hash is baked into @admin_token@).
  2. Lock that admin NFT at the @asteria.spend@ script address with
     the initial inline @AsteriaDatum {ship_counter=0,
     shipyard_policy=spacetime_hash}@ — the on-chain "asteria UTxO"
     that ships will mine and consume.
  3. Exit 0 so the player containers' @depends_on@ unblocks.

Subsequent iterations layer on:

  * deploy of all four validators as inline ref-scripts at the
    @deploy.spend@ address (so move_ship / mine / gather txs can
    reference them instead of re-attaching the script every time);
  * mint+lock of N pellet UTxOs at known coordinates;
  * the actual game loop.
-}
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.Aeson (object, (.=))
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Void (Void)
import Lens.Micro ((%~), (&), (.~), (^.))

import Cardano.Crypto.DSIGN (
    Ed25519DSIGN,
    SignKeyDSIGN,
    deriveVerKeyDSIGN,
    signedDSIGN,
 )
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Api.Tx (bodyTxL, txIdTx, witsTxL)
import Cardano.Ledger.Api.Tx.Body (outputsTxBodyL)
import Cardano.Ledger.Api.Tx.Out (
    TxOut,
    coinTxOutL,
    valueTxOutL,
 )
import Cardano.Ledger.Api.Tx.Wits (addrTxWitsL)
import Cardano.Ledger.BaseTypes (Network (Testnet))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (hashScript)
import Cardano.Ledger.Credential (
    Credential (ScriptHashObj),
    StakeReference (StakeRefNull),
 )
import Cardano.Ledger.Hashes (ScriptHash (..), extractHash)
import Cardano.Ledger.Keys (
    KeyRole (Witness),
    VKey (..),
    WitVKey (..),
    asWitness,
 )
import Cardano.Ledger.Mary.Value (
    AssetName (..),
    MaryValue (..),
    MultiAsset (..),
    PolicyID (..),
 )
import Cardano.Ledger.TxIn (TxId (..), TxIn)
import Cardano.Node.Client.Ledger (ConwayTx)
import Cardano.Node.Client.Provider (Provider (..))
import Cardano.Node.Client.Submitter (
    SubmitResult (..),
    Submitter (..),
 )
import Cardano.Node.Client.TxBuild (
    InterpretIO (..),
    TxBuild,
    attachScript,
    build,
    collateral,
    mint,
    payTo',
    spend,
 )

import Asteria.Crypto (hashToBuiltinByteString)
import Asteria.Datums (AsteriaDatum (..))
import Asteria.Provider (settingsFromEnv, withN2C)
import Asteria.Sdk (sdkReachable, sdkSometimes, sdkUnreachable)
import Asteria.Validators (adminMintScript, asteriaScript, spacetimeScript)
import Asteria.Wallet (
    WalletKey (..),
    genesisKeyPath,
    pickWalletUtxo,
    readWalletKey,
    walletAddr,
 )

main :: IO ()
main = do
    sdkReachable "asteria_bootstrap_starting" Nothing
    settings <- settingsFromEnv
    walletKey <- readWalletKey genesisKeyPath
    sdkReachable
        "asteria_bootstrap_wallet_loaded"
        ( Just $
            object ["addr" .= T.pack (show (walletAddr walletKey))]
        )
    withN2C settings $ \provider submitter -> do
        deployed <- isAlreadyDeployed provider
        if deployed
            then
                sdkSometimes
                    True
                    "asteria_bootstrap_already_deployed"
                    ( Just $
                        object
                            [ "asteria_addr"
                                .= T.pack
                                    (show (scriptAddr (hashScript asteriaScript)))
                            ]
                    )
            else do
                seed <- pickWalletUtxo provider walletKey
                sdkReachable
                    "asteria_bootstrap_seed_picked"
                    ( Just $
                        object
                            [ "seed_coin"
                                .= unCoin (snd seed ^. coinTxOutL)
                            ]
                    )
                result <- try (createAsteria provider submitter walletKey seed)
                case result of
                    Left (e :: SomeException) -> do
                        sdkUnreachable
                            "asteria_bootstrap_create_asteria_failed"
                            (Just $ object ["error" .= T.pack (show e)])
                        error (show e)
                    Right () ->
                        sdkSometimes True "asteria_bootstrap_asteria_created" Nothing
    sdkReachable "asteria_bootstrap_completed" Nothing

{- | Returns 'True' if the asteria-deployed state already exists on
chain. The check is conservative: any UTxO at the asteria spend
address that carries one unit of @(admin_mint_hash, "asteriaAdmin")@
counts as deployed.

This is the first line of idempotence — Antithesis can restart the
bootstrap container at any time, and subsequent invocations must not
re-mint the admin NFT or re-create the Asteria UTxO. The Plutus
admin_mint policy is currently always-true (PR #67's iteration-5b
placeholder), so chain-level uniqueness is *not* enforced; the next
PR replaces admin_mint with a one-shot policy parameterised on a
seed @OutputReference@. Until then this Haskell-side check, plus
Antithesis's @serial_driver_@ scheduling, is the contract.
-}
isAlreadyDeployed :: Provider IO -> IO Bool
isAlreadyDeployed provider = do
    let asteriaAddr = scriptAddr (hashScript asteriaScript)
        adminPolicy = PolicyID (hashScript adminMintScript)
        adminName = AssetName "asteriaAdmin"
    utxos <- queryUTxOs provider asteriaAddr
    pure $ any (hasAsset adminPolicy adminName . snd) utxos
  where
    hasAsset pid an out =
        case out ^. valueTxOutL of
            MaryValue _ (MultiAsset assets) ->
                case Map.lookup pid assets of
                    Just inner -> Map.findWithDefault 0 an inner > 0
                    Nothing -> False

{- | Build, sign, submit the asteria-creation tx and wait for the
ship-counter=0 UTxO to land at the asteria spend address.
-}
createAsteria ::
    Provider IO ->
    Submitter IO ->
    WalletKey ->
    (TxIn, TxOut ConwayEra) ->
    IO ()
createAsteria provider submitter wk seed@(seedIn, _) = do
    pp <- queryProtocolParams provider
    let asteriaAddr = scriptAddr (hashScript asteriaScript)
        adminPolicy = PolicyID (hashScript adminMintScript)
        adminName = AssetName "asteriaAdmin"
        spacetimeHashBs = case hashScript spacetimeScript of
            ScriptHash h -> hashToBuiltinByteString h
        initialDatum =
            AsteriaDatum
                { adShipCounter = 0
                , adShipyardPolicy = spacetimeHashBs
                }
        asteriaValue =
            MaryValue (Coin 5_000_000) $
                MultiAsset $
                    Map.singleton adminPolicy $
                        Map.singleton adminName 1
        prog :: TxBuild NoQ Void ()
        prog = do
            _ <- spend seedIn
            collateral seedIn
            attachScript adminMintScript
            mint adminPolicy (Map.singleton adminName 1) ()
            _ <- payTo' asteriaAddr asteriaValue initialDatum
            pure ()
        eval tx =
            fmap
                (Map.map (either (Left . show) Right))
                (evaluateTx provider tx)
        interpret :: InterpretIO NoQ
        interpret = InterpretIO $ \case {}
    built <- build pp interpret eval [seed] [] (walletAddr wk) prog
    case built of
        Left err -> error ("build: " <> show err)
        Right tx -> do
            let signed = addKeyWitness (wkSignKey wk) tx
                outs = toList (signed ^. bodyTxL . outputsTxBodyL)
            sdkReachable
                "asteria_bootstrap_tx_built"
                (Just $ object ["outputs" .= length outs])
            r <- submitTx submitter signed
            case r of
                Submitted _ ->
                    -- Cluster cold-start can take a while to forge
                    -- + propagate the first non-genesis block. Give
                    -- it 3 minutes before declaring failure.
                    waitForAsteria
                        provider
                        asteriaAddr
                        adminPolicy
                        adminName
                        180
                Rejected reason ->
                    error ("submit rejected: " <> show reason)

scriptAddr :: ScriptHash -> Addr
scriptAddr h = Addr Testnet (ScriptHashObj h) StakeRefNull

waitForAsteria ::
    Provider IO ->
    Addr ->
    PolicyID ->
    AssetName ->
    Int ->
    IO ()
waitForAsteria provider addr pol an attempts
    | attempts <= 0 = error "timed out waiting for asteria UTxO"
    | otherwise = do
        outs <- queryUTxOs provider addr
        if any (hasAsset pol an . snd) outs
            then pure ()
            else do
                threadDelay 1_000_000
                waitForAsteria provider addr pol an (attempts - 1)
  where
    hasAsset p n out =
        let MaryValue _ (MultiAsset ma) = out ^. valueTxOutL
            polMap = Map.findWithDefault Map.empty p ma
            qty = Map.findWithDefault 0 n polMap
         in qty > 0

-- | Phantom query GADT — bootstrap has no @ctx@ queries.
data NoQ a

addKeyWitness ::
    SignKeyDSIGN Ed25519DSIGN -> ConwayTx -> ConwayTx
addKeyWitness sk tx =
    tx & witsTxL . addrTxWitsL %~ Set.union (Set.singleton w)
  where
    TxId h = txIdTx tx
    vk = VKey (deriveVerKeyDSIGN sk)
    w = WitVKey (asWitness vk) (signedDSIGN () (extractHash h) sk)
