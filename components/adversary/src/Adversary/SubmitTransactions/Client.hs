{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NumericUnderscores #-}

module Adversary.SubmitTransactions.Client where

import Adversary.ChainSync.Codec (Block, ccfg, version)
import Adversary.ChainSync.Connection (maximumMiniProtocolLimits, resolve)
import Adversary.SubmitTransactions.Log (SubmitLog (..))
import Adversary.SubmitTransactions.Util (TxId', mkTxN2N)
import Cardano.Ledger.Alonzo.Tx ()
import Codec.CBOR.Decoding (Decoder, decodeBytes)
import Codec.CBOR.Encoding (Encoding, encodePreEncoded)
import Codec.Serialise (DeserialiseFailure)
import Control.Concurrent.Class.MonadSTM.Strict (atomically, readTBQueue, tryReadTBQueue)
import Control.Concurrent.Class.MonadSTM.Strict.TBQueue (StrictTBQueue)
import Control.Concurrent.Class.MonadSTM.Strict.TVar (modifyTVar, newTVarIO, readTVarIO)
import Control.Exception (SomeException)
import Control.Tracer (Tracer, traceWith)
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
import Ouroboros.Consensus.Node.Serialisation (decodeNodeToNode, encodeNodeToNode)
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
    keepAliveMiniProtocolNum,
    nodeToNodeCodecCBORTerm,
    nodeToNodeHandshakeCodec,
    simpleSingletonVersions,
    txSubmissionMiniProtocolNum,
  )
import Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec, noTimeLimitsHandshake)
import Ouroboros.Network.Protocol.KeepAlive.Client (KeepAliveClient (..), KeepAliveClientSt (..), keepAliveClientPeer)
import Ouroboros.Network.Protocol.KeepAlive.Codec (codecKeepAlive_v2)
import Ouroboros.Network.Protocol.KeepAlive.Type (Cookie (..))
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
import Control.Concurrent (threadDelay)

-- | Connect to a node-to-node tx submission server, and runs the given application
-- to submit transactions.
runTxSubmissionApplication ::
  -- | tracer
  Tracer IO SubmitLog ->
  -- | The network magic
  NetworkMagic ->
  -- | host
  String ->
  -- | port
  PortNumber ->
  StrictTBQueue IO (TxId', LazyByteString) ->
  IO (Either SomeException (Either () Void))
runTxSubmissionApplication tracer magic peerName peerPort queue = withIOManager $ \iocp -> do
  AddrInfo {addrAddress} <- resolve peerName peerPort
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
        (const $ txSubmissionToOuroboros tracer queue) -- application
    )
    Nothing
    addrAddress

txSubmissionToOuroboros ::
  Tracer IO SubmitLog ->
  -- | chainSync
  StrictTBQueue IO (TxId', LazyByteString) ->
  OuroborosApplicationWithMinimalCtx
    Mx.InitiatorMode
    addr
    LazyByteString
    IO
    ()
    Void
txSubmissionToOuroboros tracer queue =
  OuroborosApplication
    { getOuroborosApplication =
        [ MiniProtocol
            { miniProtocolNum = keepAliveMiniProtocolNum,
              miniProtocolStart = StartOnDemand,
              miniProtocolLimits = maximumMiniProtocolLimits,
              miniProtocolRun = runKeepAlive
            },
          MiniProtocol
            { miniProtocolNum = txSubmissionMiniProtocolNum,
              miniProtocolStart = StartOnDemand,
              miniProtocolLimits = maximumMiniProtocolLimits,
              miniProtocolRun = runTxSubmission
            }
        ]
    }
  where
    runTxSubmission =
      InitiatorProtocolOnly $
        mkMiniProtocolCbFromPeer $
          const
            ( contramap (NetworkLog . show) tracer,
              codecTxSubmission,
              txSubmissionClientPeer $ txSubmissionApplication tracer queue
            )

    runKeepAlive =
      InitiatorProtocolOnly $
        mkMiniProtocolCbFromPeer $
          const
            ( contramap (NetworkLog . show) tracer,
              codecKeepAlive_v2,
              keepAliveClientPeer keepAliveApplication
            )

keepAliveApplication :: KeepAliveClient IO ()
keepAliveApplication =
  KeepAliveClient $ do
    let cookie = Cookie 17
    pure $ SendMsgKeepAlive cookie (keepAlive cookie)
  where
    keepAlive (Cookie cookie) = do
      threadDelay 30_000_000
      let cookie' = Cookie $ cookie * 17
      pure $ SendMsgKeepAlive cookie' (keepAlive cookie')

txSubmissionApplication ::
  Tracer IO SubmitLog ->
  StrictTBQueue IO (TxId', LazyByteString) ->
  -- | the chain sync client application
  TxSubmissionApplication
txSubmissionApplication tracer txsVar =
  TxSubmissionClient $ do
    -- the inflight txs being tracked for acking
    -- FIXME: should be bounded in size to 10
    inflight <- newTVarIO []
    pure $ idle inflight
  where
    idle inflight =
      ClientStIdle
        { recvMsgRequestTxIds = \blocking numToAck numToReq -> do
            traceWith tracer $ ReceivedRequestTxIds (fromIntegral numToAck) (fromIntegral numToReq)
            -- drain inflight txs with acks
            atomically $ modifyTVar inflight (drop (fromIntegral numToAck))
            case blocking of
              SingBlocking -> do
                tx <- atomically $ readTBQueue txsVar
                atomically $ modifyTVar inflight (<> [tx])
                traceWith tracer $ SendingTxIds True (fst tx)
                return $ SendMsgReplyTxIds (BlockingReply $ sized tx :| []) (idle inflight)
              SingNonBlocking -> do
                mayTx <- atomically $ tryReadTBQueue txsVar
                case mayTx of
                  Nothing -> return $ SendMsgReplyTxIds (NonBlockingReply []) (idle inflight)
                  Just tx -> do
                    atomically $ modifyTVar inflight (<> [tx])
                    traceWith tracer $ SendingTxIds False (fst tx)
                    return $ SendMsgReplyTxIds (NonBlockingReply [sized tx]) (idle inflight),
          recvMsgRequestTxs = \reqTxIds -> do
            traceWith tracer $ ReceivedRequestTxs reqTxIds
            txs <- readTVarIO inflight
            let requestedTxs = map (mkTxN2N . snd) $ filter (\(tid, _) -> tid `elem` reqTxIds) txs
            traceWith tracer $ SendingRequestedTxs (length requestedTxs)
            return $ SendMsgReplyTxs requestedTxs (idle inflight)
        }

    sized (tid, tx) = (tid, SizeInBytes (fromIntegral (LBS.length tx)))

instance ShowProxy LBS.ByteString

type TxSubmissionApplication = TxSubmissionClient TxId' LazyByteString IO ()

codecTxSubmission :: Codec (TxSubmission2 TxId' LazyByteString) DeserialiseFailure IO LazyByteString
codecTxSubmission =
  codecTxSubmission2 encodeTxId decodeTxId (encodePreEncoded . LBS.toStrict) (fmap LBS.fromStrict decodeBytes)

encodeTxId :: TxId' -> Encoding
encodeTxId = encodeNodeToNode @Block ccfg version

decodeTxId :: Decoder s TxId'
decodeTxId = decodeNodeToNode @Block ccfg version
