{- |
Module      : Main
Description : Iteration-5 bootstrap: real tx submission against the cluster.

Connects to N2C, reads the genesis signing key from
@\/utxo-keys\/genesis.3.skey@, builds a no-op self-pay tx through
the @TxBuild@ DSL, signs with the genesis key, and submits.
Confirms by polling @queryUTxOs@ for the new tx output (the change
output's hash and value).

This proves the bootstrap binary can build → sign → submit → confirm
end-to-end against the antithesis cluster without exploding. The
actual asteria-shaped bootstrap (admin NFT mint, ref-script deploy,
asteria UTxO + pellets) lands in iteration 5b on top of the wallet
plumbing in 'Asteria.Wallet'.
-}
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.Aeson (object, (.=))
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Void (Void)
import Lens.Micro ((^.))

import Cardano.Ledger.Address (Addr)
import Cardano.Ledger.Api.Tx (bodyTxL)
import Cardano.Ledger.Api.Tx.Body (outputsTxBodyL)
import Cardano.Ledger.Api.Tx.Out (TxOut, coinTxOutL)
import Cardano.Ledger.BaseTypes (Inject (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Node.Client.Provider (Provider (..))
import Cardano.Node.Client.Submitter (
    SubmitResult (..),
    Submitter (..),
 )
import Cardano.Node.Client.TxBuild (
    InterpretIO (..),
    TxBuild,
    build,
    payTo,
    spend,
 )

import Asteria.Provider (settingsFromEnv, withN2C)
import Asteria.Sdk (sdkReachable, sdkSometimes, sdkUnreachable)
import Asteria.Wallet (
    WalletKey (..),
    genesisKeyPath,
    pickWalletUtxo,
    readWalletKey,
    walletAddr,
 )
import Cardano.Crypto.DSIGN (
    Ed25519DSIGN,
    SignKeyDSIGN,
    deriveVerKeyDSIGN,
    signedDSIGN,
 )
import Cardano.Ledger.Api.Tx (txIdTx, witsTxL)
import Cardano.Ledger.Api.Tx.Wits (addrTxWitsL)
import Cardano.Ledger.Hashes (extractHash)
import Cardano.Ledger.Keys (
    KeyRole (Witness),
    VKey (..),
    WitVKey (..),
    asWitness,
 )
import Cardano.Ledger.TxIn (TxId (..))
import Cardano.Node.Client.Ledger (ConwayTx)
import Data.Set qualified as Set
import Lens.Micro ((%~), (&))

main :: IO ()
main = do
    sdkReachable "asteria_bootstrap_starting" Nothing
    settings <- settingsFromEnv
    walletKey <- readWalletKey genesisKeyPath
    sdkReachable
        "asteria_bootstrap_wallet_loaded"
        ( Just $
            object
                [ "addr" .= T.pack (show (walletAddr walletKey))
                ]
        )
    withN2C settings $ \provider submitter -> do
        seed@(_, seedOut) <- pickWalletUtxo provider walletKey
        let inputCoin = seedOut ^. coinTxOutL
        sdkReachable
            "asteria_bootstrap_seed_picked"
            ( Just $
                object
                    ["seed_coin" .= unCoin inputCoin]
            )
        result <- try (selfPay provider submitter walletKey seed)
        case result of
            Left (e :: SomeException) -> do
                sdkUnreachable
                    "asteria_bootstrap_self_pay_failed"
                    ( Just $
                        object ["error" .= T.pack (show e)]
                    )
                error (show e)
            Right () ->
                sdkSometimes
                    True
                    "asteria_bootstrap_self_pay"
                    Nothing
    sdkReachable "asteria_bootstrap_completed" Nothing

selfPay ::
    Provider IO ->
    Submitter IO ->
    WalletKey ->
    (TxIn, TxOut ConwayEra) ->
    IO ()
selfPay provider submitter wk seed@(seedIn, _) = do
    pp <- queryProtocolParams provider
    let addr = walletAddr wk
        prog :: TxBuild NoQ Void ()
        prog = do
            _ <- spend seedIn
            _ <- payTo addr (inject (Coin 5_000_000))
            pure ()
        eval tx =
            fmap
                (Map.map (either (Left . show) Right))
                (evaluateTx provider tx)
        interpret :: InterpretIO NoQ
        interpret = InterpretIO $ \case {}
    built <- build pp interpret eval [seed] [] addr prog
    case built of
        Left err -> error ("build: " <> show err)
        Right tx -> do
            let signed = addKeyWitness (wkSignKey wk) tx
                outs = toList (signed ^. bodyTxL . outputsTxBodyL)
            sdkReachable
                "asteria_bootstrap_tx_built"
                ( Just $
                    object
                        ["outputs" .= length outs]
                )
            r <- submitTx submitter signed
            case r of
                Submitted _ ->
                    waitForConfirmation provider addr 60
                Rejected reason ->
                    error ("submit rejected: " <> show reason)

waitForConfirmation ::
    Provider IO -> Addr -> Int -> IO ()
waitForConfirmation provider addr attempts
    | attempts <= 0 = error "timed out waiting for confirmation"
    | otherwise = do
        outs <- queryUTxOs provider addr
        if length outs >= 1
            then pure ()
            else do
                threadDelay 1_000_000
                waitForConfirmation provider addr (attempts - 1)

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
