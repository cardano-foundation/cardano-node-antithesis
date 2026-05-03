{- |
Module      : Asteria.Wallet
Description : Genesis-key wallet for the antithesis cluster.

Reads the genesis-utxo signing key from
@\/utxo-keys\/genesis.1.skey@ (placed there by the @configurator@
container at cluster boot), decodes it, and exposes:

  * 'genesisSignKey'  — the 'SignKeyDSIGN' for signing.
  * 'genesisAddr'     — the Shelley payment address.
  * 'pickGenesisUtxo' — first UTxO sitting at that address.

The skey file is a Cardano text-envelope JSON document whose
@cborHex@ field encodes a CBOR @bytes@ value (header @5820@) holding
the raw 32-byte ed25519 secret key.
-}
module Asteria.Wallet (
    WalletKey (..),
    genesisKeyPath,
    readWalletKey,
    walletAddr,
    pickWalletUtxo,
) where

import Cardano.Crypto.DSIGN (
    Ed25519DSIGN,
    SignKeyDSIGN,
    deriveVerKeyDSIGN,
    rawDeserialiseSignKeyDSIGN,
 )
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Api.Tx.Out (TxOut, coinTxOutL, valueTxOutL)
import Cardano.Ledger.BaseTypes (Network (Testnet))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Credential (
    Credential (KeyHashObj),
    StakeReference (StakeRefNull),
 )
import Cardano.Ledger.Keys (KeyHash, KeyRole (Payment), VKey (..), hashKey)
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..))
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Node.Client.Provider (Provider (..))
import Data.Aeson (Value (Object), eitherDecodeFileStrict, (.:))
import Data.Aeson.Types (parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Lens.Micro ((^.))

-- | Loaded wallet: signing key + key hash + address.
data WalletKey = WalletKey
    { wkSignKey :: SignKeyDSIGN Ed25519DSIGN
    , wkKeyHash :: KeyHash Payment
    }

{- | Default mount-point for the antithesis cluster's genesis
signing keys. The @configurator@ container writes them here at
boot, and bootstrap / player containers mount it read-only.

We pick @genesis.3.skey@ deliberately: @tx-generator@ takes the
first available genesis key (@genesis.1@) and would race the
bootstrap for the same UTxO otherwise, surfacing as a
\"All inputs are spent\" mempool error.
-}
genesisKeyPath :: FilePath
genesisKeyPath = "/utxo-keys/genesis.3.skey"

{- | Read a Cardano text-envelope @.skey@ file and decode its
@cborHex@ into a 'WalletKey'.
-}
readWalletKey :: FilePath -> IO WalletKey
readWalletKey fp = do
    raw <- eitherDecodeFileStrict fp
    case raw of
        Left e ->
            error ("Asteria.Wallet: parse JSON " <> fp <> ": " <> e)
        Right v ->
            case parseEither (extractCborHex fp) v of
                Left e -> error e
                Right h -> pure (mkKey h)
  where
    extractCborHex path val = do
        case val of
            Object o -> do
                hexT <- o .: "cborHex"
                pure (hexT :: Text)
            _ ->
                fail
                    ( "Asteria.Wallet: expected JSON object in "
                        <> path
                    )

mkKey :: Text -> WalletKey
mkKey hexT =
    let bytes =
            either
                (error . ("Asteria.Wallet: hex decode: " <>))
                id
                (Base16.decode (BS8.pack (T.unpack hexT)))
        -- Strip the CBOR header (1 byte tag + 1 byte length for
        -- bytestrings of length 24..255). Genesis skeys are 32
        -- bytes wrapped as "5820<32 bytes>".
        keyBytes =
            if BS.length bytes == 34
                && BS.take 2 bytes == BS.pack [0x58, 0x20]
                then BS.drop 2 bytes
                else
                    error
                        ( "Asteria.Wallet: unexpected CBOR shape, "
                            <> "len="
                            <> show (BS.length bytes)
                        )
        sk =
            fromMaybe
                (error "Asteria.Wallet: rawDeserialiseSignKeyDSIGN")
                (rawDeserialiseSignKeyDSIGN keyBytes)
        vk = VKey (deriveVerKeyDSIGN sk)
        kh = hashKey vk
     in WalletKey{wkSignKey = sk, wkKeyHash = kh}

{- | Derive the Shelley payment address from a 'WalletKey'. No
stake credential — same shape the e2e harness uses.
-}
walletAddr :: WalletKey -> Addr
walletAddr wk = Addr Testnet (KeyHashObj (wkKeyHash wk)) StakeRefNull

{- | Query the provider for UTxOs at the wallet's address and
return the one with the largest pure-ada balance, preferring
UTxOs that hold no native tokens (so balanceTx's change output
doesn't have to carry token dust).

After the first spawn tx the wallet has a small change output
plus the original genesis UTxO; "first" UTxO selection picks
the change one and the next spawn fails @BalanceFailed
InsufficientFee@. Largest-pure-ada selection avoids that.

Errors if there are no UTxOs — caller is expected to call this
once the cluster has booted.
-}
pickWalletUtxo ::
    Provider IO -> WalletKey -> IO (TxIn, TxOut ConwayEra)
pickWalletUtxo provider wk = do
    let addr = walletAddr wk
    outs <- queryUTxOs provider addr
    case sortOn rank outs of
        u : _ -> pure u
        [] -> error ("Asteria.Wallet: no UTxOs at " <> show addr)
  where
    -- (hasNativeTokens, Down lovelace): pure-ada UTxOs first,
    -- then by descending lovelace.
    rank (_, out) =
        let MaryValue (Coin lov) (MultiAsset ma) = out ^. valueTxOutL
            hasTokens = not (Map.null ma)
         in (hasTokens, Down lov)
