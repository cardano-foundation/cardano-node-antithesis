module Adversary.SubmitTransactions where

import Adversary (Message (..), readOrFail, toString)
import Adversary.ChainSync.Codec (Block, ccfg, version)
import Adversary.ChainSync.Connection (maximumMiniProtocolLimits, resolve)
import Codec.Serialise (DeserialiseFailure)
import Control.Exception (SomeException)
import Control.Tracer (nullTracer, stdoutTracer)
import Data.ByteString.Lazy (LazyByteString)
import Data.Functor.Contravariant (contramap)
import Data.Void (Void)
import Network.Mux qualified as Mx
import Network.Socket (AddrInfo (..), PortNumber)
import Network.TypedProtocol.Codec (Codec)
import Ouroboros.Consensus.Cardano (CardanoBlock)
import Ouroboros.Consensus.Cardano.Block (CardanoEras, StandardCrypto)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseNodeToNode ()
import Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, GenTxId)
import Ouroboros.Network.NodeToNode (txSubmissionMiniProtocolNum)
import Ouroboros.Consensus.Node.Serialisation (decodeNodeToNode, encodeNodeToNode)
import Ouroboros.Consensus.Shelley.Eras (ConwayEra)
import Ouroboros.Consensus.Shelley.Ledger.Mempool ()
import Ouroboros.Consensus.Shelley.Node.Serialisation ()
import Ouroboros.Network.Diffusion.Configuration (PeerSharing (PeerSharingDisabled))
import Ouroboros.Network.Handshake.Acceptable (Acceptable (..))
import Ouroboros.Network.Handshake.Queryable (queryVersion)
import Ouroboros.Network.IOManager (withIOManager)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mux
  ( MiniProtocol (..),
    MiniProtocolNum (..),
    OuroborosApplication (..),
    OuroborosApplicationWithMinimalCtx,
    RunMiniProtocol (InitiatorProtocolOnly),
    StartOnDemandOrEagerly (StartOnDemand),
    mkMiniProtocolCbFromPeer,
  )
import Ouroboros.Network.NodeToNode
  ( DiffusionMode (InitiatorOnlyDiffusionMode),
    NodeToNodeVersion (NodeToNodeV_14),
    NodeToNodeVersionData (..),
    nodeToNodeCodecCBORTerm,
    nodeToNodeHandshakeCodec,
    nullNetworkConnectTracers,
    simpleSingletonVersions,
  )
import Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec, noTimeLimitsHandshake)
import Ouroboros.Network.Protocol.TxSubmission2.Client (TxSubmissionClient (..), txSubmissionClientPeer)
import Ouroboros.Network.Protocol.TxSubmission2.Codec (codecTxSubmission2)
import Ouroboros.Network.Protocol.TxSubmission2.Type (TxSubmission2)
import Ouroboros.Network.Snocket (makeSocketBearer, socketSnocket)
import Ouroboros.Network.Socket (ConnectToArgs (..), HandshakeCallbacks (..), connectToNode, debuggingNetworkConnectTracers)

submitTxs :: [String] -> IO Message
submitTxs = \case
    args@(magicArg : host : port : txFiles) -> do
        putStrLn $ toString $ Startup args
        let magic = NetworkMagic {unNetworkMagic = readOrFail "magic" magicArg}
        result <- runTxSubmissionApplication magic host (readOrFail "port" port) (const mkTxSubmissionApplication)
        return $ Completed []

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
  (NodeToNodeVersionData -> TxSubmissionApplication) ->
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
        (txSubmissionToOuroboros . application) -- application
    )
    Nothing
    addrAddress

mkTxSubmissionApplication ::
  -- | the chain sync client application
  TxSubmissionApplication
mkTxSubmissionApplication = TxSubmissionClient $ undefined

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
  where
    encodeTxId = encodeNodeToNode @Block ccfg version
    decodeTxId = decodeNodeToNode @Block ccfg version

    encodeTx = encodeNodeToNode @Block ccfg version
    decodeTx = decodeNodeToNode @Block ccfg version
