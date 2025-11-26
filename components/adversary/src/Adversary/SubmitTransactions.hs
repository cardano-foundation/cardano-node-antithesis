{-# OPTIONS_GHC -Wno-orphans #-}

module Adversary.SubmitTransactions where

import Adversary (Message (..), readOrFail, toString)
import Adversary.ChainSync.Codec (Block, ccfg, version)
import Adversary.ChainSync.Connection (maximumMiniProtocolLimits, resolve)
import Cardano.Crypto.Hash (Blake2b_256, hashWith)
import Cardano.Ledger.Alonzo.Tx ()
import Cardano.Ledger.Binary (decodeFull)
import Cardano.Ledger.Binary.Plain (Term (TList))
import Cardano.Ledger.Core (eraProtVerHigh)
import Cardano.Ledger.Hashes (unsafeMakeSafeHash)
import Cardano.Ledger.TxIn qualified as Ledger
import Codec.CBOR.Decoding (Decoder, decodeBytes)
import Codec.CBOR.Encoding (Encoding, encodePreEncoded)
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Term (decodeTerm, encodeTerm)
import Codec.CBOR.Write (toLazyByteString)
import Codec.Serialise (DeserialiseFailure)
import Control.Concurrent.Class.MonadSTM.Strict (atomically)
import Control.Concurrent.Class.MonadSTM.Strict.TVar (StrictTVar, modifyTVar, newTVarIO, readTVar, readTVarIO)
import Control.Exception (SomeException)
import Control.Tracer (stdoutTracer)
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString.Base16.Lazy qualified as Hex
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Functor.Contravariant (contramap)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Void (Void)
import Network.Mux qualified as Mx
import Network.Socket (AddrInfo (..), PortNumber)
import Network.TypedProtocol.Codec (Codec)
import Ouroboros.Consensus.Cardano.Block (GenTx (GenTxConway), TxId (GenTxIdConway))
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseNodeToNode ()
import Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId, HasTxId (txId))
import Ouroboros.Consensus.Node.Serialisation (decodeNodeToNode, encodeNodeToNode)
import Ouroboros.Consensus.Shelley.Eras (ConwayEra)
import Ouroboros.Consensus.Shelley.Ledger.Mempool (TxId (ShelleyTxId), mkShelleyTx)
import Ouroboros.Consensus.Shelley.Node.Serialisation ()
import Ouroboros.Network.Diffusion.Configuration (DiffusionMode (..), PeerSharing (PeerSharingDisabled))
import Ouroboros.Network.Handshake.Acceptable (Acceptable (..))
import Ouroboros.Network.Handshake.Queryable (queryVersion)
import Ouroboros.Network.IOManager (withIOManager)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mux
  ( MiniProtocol (..),
    OuroborosApplication (..),
    OuroborosApplicationWithMinimalCtx,
    RunMiniProtocol (..),
    StartOnDemandOrEagerly (..),
    mkMiniProtocolCbFromPeer,
  )
import Ouroboros.Network.NodeToNode
  ( NodeToNodeVersion (NodeToNodeV_14),
    NodeToNodeVersionData (..),
    nodeToNodeCodecCBORTerm,
    nodeToNodeHandshakeCodec,
    simpleSingletonVersions,
    txSubmissionMiniProtocolNum,
  )
import Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec, noTimeLimitsHandshake)
import Ouroboros.Network.Protocol.TxSubmission2.Client
  ( BlockingReplyList (..),
    ClientStIdle (..),
    ClientStTxIds (..),
    ClientStTxs (..),
    SingBlockingStyle (..),
    TxSubmissionClient (..),
    txSubmissionClientPeer,
  )
import Ouroboros.Network.Protocol.TxSubmission2.Codec (codecTxSubmission2)
import Ouroboros.Network.Protocol.TxSubmission2.Type (SizeInBytes (..), TxSubmission2)
import Ouroboros.Network.Snocket (makeSocketBearer, socketSnocket)
import Ouroboros.Network.Socket (ConnectToArgs (..), HandshakeCallbacks (..), connectToNode, debuggingNetworkConnectTracers)
import Ouroboros.Network.Util.ShowProxy (ShowProxy)

submitTxs :: [String] -> IO Message
submitTxs = \case
  args@(magicArg : host : port : hexEncodedTxFiles) -> do
    putStrLn $ toString $ Startup args
    let magic = NetworkMagic {unNetworkMagic = readOrFail "magic" magicArg}
    txs <-
      mapM
        ( \file -> do
            content <- LBS.readFile file
            case Hex.decode content of
              Left err -> error $ "Failed to deserialise transaction from file " ++ file ++ ": " ++ show err
              Right tx -> case mkTxId tx of
                Left err -> error $ "Failed to compute transaction id from file " ++ file ++ ": " ++ show err
                Right txid -> return (txid, tx)
        )
        hexEncodedTxFiles
        >>= newTVarIO
    _ <- runTxSubmissionApplication magic host (readOrFail "port" port) (mkTxSubmissionApplication txs)
    return $ Completed []
  _ -> pure $ Usage "Usage: submit-txs <magic> <host> <port> <tx-file1> <tx-file2> ..."

fromHex :: LBS.ByteString -> LBS.ByteString
fromHex = either (error . ("Failed to decode hex: " ++)) id . Hex.decode

getTxId :: Tx -> TxId'
getTxId = txId

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

type TxSubmissionApplication = TxSubmissionClient TxId' LazyByteString IO ()

type TxId' = GenTxId Block

type Tx = GenTx Block

-- | Connect to a node-to-node tx submission server, and runs the given application
-- to submit transactions.
runTxSubmissionApplication ::
  -- | The network magic
  NetworkMagic ->
  -- | host
  String ->
  -- | port
  PortNumber ->
  -- | application
  TxSubmissionApplication ->
  IO (Either SomeException (Either () Void))
runTxSubmissionApplication magic peerName peerPort application = withIOManager $ \iocp -> do
  AddrInfo {addrAddress} <- resolve peerName peerPort
  print addrAddress
  connectToNode -- withNode
    (socketSnocket iocp) -- TCP
    makeSocketBearer
    ConnectToArgs
      { ctaHandshakeCodec = nodeToNodeHandshakeCodec,
        ctaHandshakeTimeLimits = noTimeLimitsHandshake,
        ctaVersionDataCodec =
          cborTermVersionDataCodec
            nodeToNodeCodecCBORTerm,
        ctaConnectTracers = debuggingNetworkConnectTracers,
        ctaHandshakeCallbacks =
          HandshakeCallbacks
            { acceptCb = acceptableVersion,
              queryCb = queryVersion
            }
      }
    mempty -- socket options
    ( simpleSingletonVersions
        NodeToNodeV_14
        ( NodeToNodeVersionData
            { networkMagic = magic,
              diffusionMode = InitiatorOnlyDiffusionMode,
              peerSharing = PeerSharingDisabled,
              query = False
            }
        )
        (const $ txSubmissionToOuroboros application) -- application
    )
    Nothing
    addrAddress

mkTxSubmissionApplication ::
  StrictTVar IO [(TxId', LazyByteString)] ->
  -- | the chain sync client application
  TxSubmissionApplication
mkTxSubmissionApplication txsVar =
  TxSubmissionClient $ pure idle
  where
    idle =
      ClientStIdle
        { recvMsgRequestTxIds = \blocking numToAck numToReq -> do
            putStrLn $
              "Received request for tx ids: "
                ++ show blocking
                ++ "(ack: "
                ++ show numToAck
                ++ ", req: "
                ++ show numToReq
                ++ ")"
            txIds <- atomically $ do
              txs <- readTVar txsVar
              let toSend =
                    drop (fromIntegral numToAck) txs
                      & take (fromIntegral numToReq)
              let txIdsWithSizes =
                    map
                      ( \(tid, tx) ->
                          ( tid,
                            SizeInBytes (fromIntegral (LBS.length tx))
                          )
                      )
                      toSend
              modifyTVar txsVar (drop (fromIntegral numToAck))
              return txIdsWithSizes
            putStrLn $ "Sending " ++ show txIds
            case blocking of
              SingBlocking -> case txIds of
                [] -> return $ SendMsgDone ()
                (t : ts) -> return $ SendMsgReplyTxIds (BlockingReply $ t :| ts) idle
              SingNonBlocking -> return $ SendMsgReplyTxIds (NonBlockingReply txIds) idle,
          recvMsgRequestTxs = \reqTxIds -> do
            putStrLn $ "Received request for txs ids: " ++ show reqTxIds
            txs <- readTVarIO txsVar
            let requestedTxs = map snd $ filter (\(tid, _) -> tid `elem` reqTxIds) txs
            putStrLn $ "Sending " ++ show (length requestedTxs) ++ " requested txs"
            return $ SendMsgReplyTxs requestedTxs idle
        }

txSubmissionToOuroboros ::
  -- | chainSync
  TxSubmissionApplication ->
  OuroborosApplicationWithMinimalCtx
    Mx.InitiatorMode
    addr
    LazyByteString
    IO
    ()
    Void
txSubmissionToOuroboros txSubmissionapp =
  OuroborosApplication
    { getOuroborosApplication =
        [ MiniProtocol
            { miniProtocolNum = txSubmissionMiniProtocolNum,
              miniProtocolStart = StartOnDemand,
              miniProtocolLimits = maximumMiniProtocolLimits,
              miniProtocolRun = run
            }
        ]
    }
  where
    run =
      InitiatorProtocolOnly $
        mkMiniProtocolCbFromPeer $
          const
            ( contramap show stdoutTracer, -- tracer
              codecTxSubmission,
              txSubmissionClientPeer txSubmissionapp
            )

instance ShowProxy LBS.ByteString

codecTxSubmission :: Codec (TxSubmission2 TxId' LazyByteString) DeserialiseFailure IO LazyByteString
codecTxSubmission =
  codecTxSubmission2 encodeTxId decodeTxId (encodePreEncoded . LBS.toStrict) (fmap LBS.fromStrict decodeBytes)

encodeTxId :: TxId' -> Encoding
encodeTxId = encodeNodeToNode @Block ccfg version

decodeTxId :: Decoder s TxId'
decodeTxId = decodeNodeToNode @Block ccfg version
