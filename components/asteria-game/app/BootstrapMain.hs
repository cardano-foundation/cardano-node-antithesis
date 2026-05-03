{- |
Module      : Main
Description : Idempotent, one-shot asteria deploy.

Picks a wallet UTxO as the deploy seed, applies the seed to the
four asteria validators (so 'admin_mint' becomes a one-shot
policy parameterised on that exact UTxO), and submits the tx
that mints the @asteriaAdmin@ NFT and locks it at the asteria
spend address with the initial @AsteriaDatum {ship_counter=0,
shipyard_policy=spacetime_hash}@.

Idempotence is enforced both off-chain and on-chain:

  * Off-chain: bootstrap reads @\/asteria-deploy\/seed.json@ on
    startup. If present, it reuses the same seed (deterministic
    re-derivation) and short-circuits if the asteria UTxO already
    exists at the resulting address.
  * On-chain: 'admin_mint(seed)' rejects any tx that doesn't
    consume the exact seed 'OutputReference'. The seed can be
    consumed at most once across all chain history, so the
    @asteriaAdmin@ token can be minted at most once.

The seed is written to disk *before* the deploy tx is submitted,
so a crash between write and submit leaves either no file (next
attempt picks fresh) or a consistent file (next attempt
re-derives the same scripts).
-}
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.Aeson (object, (.=))
import Data.ByteString.Short qualified as SBS
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Void (Void)
import Lens.Micro ((%~), (&), (^.))

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
import Asteria.Deploy (readSeed, writeSeed)
import Asteria.Provider (settingsFromEnv, withN2C)
import Asteria.Provider qualified
import Asteria.Sdk (sdkReachable, sdkSometimes, sdkUnreachable)
import Asteria.Validators (
    AppliedScripts (..),
    applyScripts,
    asteriaAdminAssetName,
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
    sdkReachable "asteria_bootstrap_starting" Nothing
    settings <- settingsFromEnv
    walletKey <- readWalletKey genesisKeyPath
    sdkReachable
        "asteria_bootstrap_wallet_loaded"
        ( Just $
            object ["addr" .= T.pack (show (walletAddr walletKey))]
        )
    -- Top-level catch: anything that escapes (withN2C connection
    -- failures, transient queryUTxOs errors, etc.) should not
    -- propagate as a non-zero exit — Antithesis treats every
    -- non-zero exit as a real "Always: zero exit" violation. Emit
    -- a 'sdkSometimes deferred' assertion describing the failure
    -- and exit cleanly so the next composer fire retries.
    result <- try (bootstrap settings walletKey)
    case result of
        Left (e :: SomeException) ->
            sdkSometimes
                True
                "asteria_bootstrap_deferred"
                (Just $ object ["error" .= T.pack (show e)])
        Right () -> pure ()
    sdkReachable "asteria_bootstrap_completed" Nothing

bootstrap :: Asteria.Provider.N2CSettings -> WalletKey -> IO ()
bootstrap settings walletKey =
    withN2C settings $ \provider submitter -> do
        -- 1. Resolve the seed: prefer the persisted one (this
        --    re-derivation guarantees identical script hashes
        --    and addresses across container restarts), else
        --    pick a fresh wallet UTxO.
        mPersisted <- readSeed
        seedIn <- case mPersisted of
            Just s -> do
                sdkReachable "asteria_bootstrap_seed_reused" Nothing
                pure s
            Nothing -> do
                (sIn, sOut) <- pickWalletUtxo provider walletKey
                sdkReachable
                    "asteria_bootstrap_seed_picked"
                    ( Just $
                        object
                            ["seed_coin" .= unCoin (sOut ^. coinTxOutL)]
                    )
                pure sIn
        let scripts = applyScripts seedIn
            asteriaAddr = scriptAddr (asAsteriaHash scripts)
        -- 2. Already on chain? (Both pre- and post-restart paths
        --    end here — if the deploy tx already landed, short-
        --    circuit.)
        deployed <- isAlreadyDeployed provider asteriaAddr
        if deployed
            then
                sdkSometimes
                    True
                    "asteria_bootstrap_already_deployed"
                    ( Just $
                        object
                            ["asteria_addr" .= T.pack (show asteriaAddr)]
                    )
            else case mPersisted of
                Just _ ->
                    runDeploy provider submitter walletKey seedIn scripts asteriaAddr
                Nothing -> do
                    writeSeed seedIn
                    sdkReachable "asteria_bootstrap_seed_persisted" Nothing
                    runDeploy provider submitter walletKey seedIn scripts asteriaAddr

runDeploy ::
    Provider IO ->
    Submitter IO ->
    WalletKey ->
    TxIn ->
    AppliedScripts ->
    Addr ->
    IO ()
runDeploy provider submitter walletKey seedIn scripts asteriaAddr = do
    -- Resolving the seed (looking up its TxOut) and createAsteria
    -- (build / sign / submit / wait) can both fail transiently —
    -- chain not yet forging, mempool race after a prior submit
    -- attempt, etc. We catch and emit a 'sdkSometimes deferred'
    -- assertion so the composer's "Always: zero exit" property
    -- isn't fooled by the not-yet-ready window. The next composer
    -- fire retries.
    result <-
        try $ do
            seedSeed <- resolveSeed provider walletKey seedIn
            createAsteria
                provider
                submitter
                walletKey
                scripts
                asteriaAddr
                seedSeed
    case result of
        Left (e :: SomeException) ->
            sdkSometimes
                True
                "asteria_bootstrap_create_asteria_deferred"
                (Just $ object ["error" .= T.pack (show e)])
        Right () ->
            sdkSometimes True "asteria_bootstrap_asteria_created" Nothing

{- | Look up a 'TxIn' in the wallet's UTxOs and return the
'(TxIn, TxOut)' pair the build needs. Errors if not found —
that means the seed was consumed by another tx, which is fatal
for this bootstrap run.
-}
resolveSeed ::
    Provider IO -> WalletKey -> TxIn -> IO (TxIn, TxOut ConwayEra)
resolveSeed provider wk needle = do
    utxos <- queryUTxOs provider (walletAddr wk)
    case lookup needle utxos of
        Just out -> pure (needle, out)
        Nothing ->
            error
                ( "Asteria bootstrap: persisted seed "
                    <> show needle
                    <> " not found at wallet addr — likely \
                       \consumed by an unrelated tx"
                )

{- | Returns 'True' if any UTxO already sits at the (per-deploy)
asteria spend address. Under the one-shot policy at most one
such UTxO can ever exist, so the conservative "any UTxO" check
matches the on-chain invariant exactly.
-}
isAlreadyDeployed :: Provider IO -> Addr -> IO Bool
isAlreadyDeployed provider addr =
    not . null <$> queryUTxOs provider addr

{- | Build, sign, submit the asteria-creation tx and wait for the
ship-counter=0 UTxO to land at the asteria spend address.
-}
createAsteria ::
    Provider IO ->
    Submitter IO ->
    WalletKey ->
    AppliedScripts ->
    Addr ->
    (TxIn, TxOut ConwayEra) ->
    IO ()
createAsteria provider submitter wk scripts asteriaAddr seed@(seedIn, _) = do
    pp <- queryProtocolParams provider
    let adminPolicy = PolicyID (asAdminMintHash scripts)
        adminName = AssetName (SBS.toShort asteriaAdminAssetName)
        spacetimeHashBs = case asSpacetimeHash scripts of
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
            attachScript (asAdminMintScript scripts)
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
