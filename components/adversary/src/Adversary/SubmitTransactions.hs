module Adversary.SubmitTransactions where

import Adversary (Message (..), readOrFail, toString)
import Adversary.ChainSync.Codec (Block, ccfg, version)
import Adversary.ChainSync.Connection (maximumMiniProtocolLimits, resolve)
import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Encoding (Encoding)
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Codec.Serialise (DeserialiseFailure)
import Control.Concurrent.Class.MonadSTM.Strict.TVar (StrictTVar, newTVarIO, readTVarIO)
import Control.Exception (SomeException)
import Control.Tracer (stdoutTracer)
import Data.Bifunctor (first)
import Data.ByteString.Base16.Lazy qualified as Hex
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.Contravariant (contramap)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Void (Void)
import Network.Mux qualified as Mx
import Network.Socket (AddrInfo (..), PortNumber)
import Network.TypedProtocol.Codec (Codec)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseNodeToNode ()
import Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, GenTxId, HasTxId (txId))
import Ouroboros.Consensus.Node.Serialisation (decodeNodeToNode, encodeNodeToNode)
import Ouroboros.Consensus.Shelley.Ledger.Mempool ()
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
  ( DiffusionMode (InitiatorOnlyDiffusionMode),
    NodeToNodeVersion (NodeToNodeV_14),
    NodeToNodeVersionData (..),
    nodeToNodeCodecCBORTerm,
    nodeToNodeHandshakeCodec,
    simpleSingletonVersions,
    txSubmissionMiniProtocolNum,
  )
import Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec, noTimeLimitsHandshake)
import Ouroboros.Network.Protocol.TxSubmission2.Client (BlockingReplyList (..), ClientStIdle (..), ClientStTxIds (..), ClientStTxs (..), SingBlockingStyle (..), TxSubmissionClient (..), txSubmissionClientPeer)
import Ouroboros.Network.Protocol.TxSubmission2.Codec (codecTxSubmission2)
import Ouroboros.Network.Protocol.TxSubmission2.Type (SizeInBytes (..), TxSubmission2)
import Ouroboros.Network.Snocket (makeSocketBearer, socketSnocket)
import Ouroboros.Network.Socket (ConnectToArgs (..), HandshakeCallbacks (..), connectToNode, debuggingNetworkConnectTracers)

submitTxs :: [String] -> IO Message
submitTxs = \case
  args@(magicArg : host : port : hexEncodedTxFiles) -> do
    putStrLn $ toString $ Startup args
    let magic = NetworkMagic {unNetworkMagic = readOrFail "magic" magicArg}
    txs <-
      mapM
        ( \file -> do
            content <- LBS.readFile file
            case Hex.decode content >>= first show . deserialiseFromBytes decodeTx of
              Left err -> error $ "Failed to deserialise transaction from file " ++ file ++ ": " ++ show err
              Right tx -> return $ snd tx
        )
        hexEncodedTxFiles
        >>= newTVarIO
    _ <- runTxSubmissionApplication magic host (readOrFail "port" port) (mkTxSubmissionApplication txs)
    return $ Completed []
  _ -> pure $ Usage "Usage: submit-txs <magic> <host> <port> <tx-file1> <tx-file2> ..."

type TxSubmissionApplication = TxSubmissionClient TxId Tx IO ()

type TxId = GenTxId Block

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
  StrictTVar IO [Tx] ->
  -- | the chain sync client application
  TxSubmissionApplication
mkTxSubmissionApplication txsVar =
  TxSubmissionClient $ pure idle
  where
    idle =
      ClientStIdle
        { recvMsgRequestTxIds = \blocking _numToAck _numToReq -> do
            putStrLn $ "Received request for tx ids: " ++ show blocking
            txs <- readTVarIO txsVar
            let txIdsWithSizes =
                  map
                    ( \tx ->
                        ( txId tx,
                          SizeInBytes (fromIntegral (LBS.length (toLazyByteString $ encodeTx tx)))
                        )
                    )
                    txs
            putStrLn $ "Sending " ++ show txIdsWithSizes
            case blocking of
              SingBlocking -> case txIdsWithSizes of
                [] -> return $ SendMsgDone ()
                (t : ts) -> return $ SendMsgReplyTxIds (BlockingReply $ t :| ts) idle
              SingNonBlocking -> return $ SendMsgReplyTxIds (NonBlockingReply txIdsWithSizes) idle,
          recvMsgRequestTxs = \reqTxIds -> do
            putStrLn $ "Received request for txs ids: " ++ show reqTxIds
            txs <- readTVarIO txsVar
            let requestedTxs = filter (\tx -> txId tx `elem` reqTxIds) txs
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

codecTxSubmission :: Codec (TxSubmission2 TxId Tx) DeserialiseFailure IO LazyByteString
codecTxSubmission =
  codecTxSubmission2 encodeTxId decodeTxId encodeTx decodeTx

encodeTxId :: TxId -> Encoding
encodeTxId = encodeNodeToNode @Block ccfg version

decodeTxId :: Decoder s TxId
decodeTxId = decodeNodeToNode @Block ccfg version

encodeTx :: Tx -> Encoding
encodeTx = encodeNodeToNode @Block ccfg version

decodeTx :: Decoder s Tx
decodeTx = decodeNodeToNode @Block ccfg version
