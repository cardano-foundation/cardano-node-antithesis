{-# OPTIONS_GHC -Wno-orphans #-}

module Adversary.SubmitTransactions.Util where

import Adversary.ChainSync.Codec (Block, ccfg, version)
import Cardano.Crypto.Hash (Blake2b_256, hashWith)
import Cardano.Ledger.Alonzo.Tx ()
import Cardano.Ledger.Binary (decodeFull)
import Cardano.Ledger.Binary.Plain (Term (TList))
import Cardano.Ledger.Core (eraProtVerHigh)
import Cardano.Ledger.Hashes (unsafeMakeSafeHash)
import Cardano.Ledger.TxIn qualified as Ledger
import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Encoding (encodeBytes, encodeListLen, encodeTag, encodeWord)
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Term (decodeTerm, encodeTerm)
import Codec.CBOR.Write (toLazyByteString)
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString.Base16.Lazy qualified as Hex
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Coerce (coerce)
import Data.Function ((&))
import Ouroboros.Consensus.Cardano.Block (GenTx (GenTxConway), TxId (GenTxIdConway))
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseNodeToNode ()
import Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId, HasTxId (txId))
import Ouroboros.Consensus.Node.Serialisation (encodeNodeToNode)
import Ouroboros.Consensus.Shelley.Eras (ConwayEra)
import Ouroboros.Consensus.Shelley.Ledger.Mempool (TxId (ShelleyTxId), mkShelleyTx)
import Ouroboros.Consensus.Shelley.Node.Serialisation ()
import Ouroboros.Network.Protocol.TxSubmission2.Client
  ( TxSubmissionClient (..),
  )

fromHex :: LBS.ByteString -> LBS.ByteString
fromHex = either (error . ("Failed to decode hex: " ++)) id . Hex.decode

getTxId :: Tx -> TxId'
getTxId = txId

encodeN2N :: Tx -> LBS.ByteString
encodeN2N = toLazyByteString . encodeNodeToNode @Block ccfg version

-- | Encode a raw transaction bytes into node-to-node format
--
-- The N2N format wraps the raw transaction bytes into a CBOR array with 2 elements:
-- - the era number (6 == Conway)
-- - the raw transaction bytes as a byte string prefixed with tag 24 indicating embedded CBOR (see https://www.rfc-editor.org/rfc/rfc8949.html#name-encoded-cbor-data-item)
mkTxN2N :: LBS.ByteString -> LBS.ByteString
mkTxN2N txBytes =
  toLazyByteString $
    encodeListLen 2 <> encodeWord 6 <> encodeTag 24 <> encodeBytes (LBS.toStrict txBytes)

-- | Compute the transaction id from raw transaction bytes.
mkTxId :: LBS.ByteString -> Either String TxId'
mkTxId txBytes =
  deserialiseFromBytes dec txBytes
    & bimap (("Failed to decode transaction for tx id: " ++) . show) snd
  where
    dec :: forall s. Decoder s TxId'
    dec =
      decodeTerm >>= \case
        TList (body : _) -> do
          let bodyBytes = toLazyByteString (encodeTerm body)
          pure $
            GenTxIdConway $
              ShelleyTxId $
                Ledger.TxId $
                  unsafeMakeSafeHash $
                    coerce $
                      hashWith @Blake2b_256 id (LBS.toStrict bodyBytes)
        _ -> fail "Expected a 'list' term for transaction"

mkGenTx :: LBS.ByteString -> Either String Tx
mkGenTx txBytes = bimap show (GenTxConway . mkShelleyTx @ConwayEra) $ decode txBytes
  where
    decode = decodeFull (eraProtVerHigh @ConwayEra)

type TxId' = GenTxId Block

type Tx = GenTx Block
