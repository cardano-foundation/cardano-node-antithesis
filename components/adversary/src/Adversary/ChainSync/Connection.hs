module Adversary.ChainSync.Connection
  ( runChainSyncApplication,
    HeaderHash,
    ChainSyncApplication,
    resolve,
    maximumMiniProtocolLimits,
  )
where

import Adversary.ChainSync.Codec
  ( Block,
    Header,
    Point,
    Tip,
    codecChainSync,
  )
import Control.Exception (SomeException)
import Control.Tracer (nullTracer)
import Data.ByteString.Lazy (LazyByteString)
import Data.List.NonEmpty qualified as NE
import Data.Void (Void)
import Network.Mux qualified as Mx
import Network.Socket
  ( AddrInfo (..),
    AddrInfoFlag (AI_PASSIVE),
    PortNumber,
    SocketType (Stream),
    defaultHints,
    getAddrInfo,
  )
import Ouroboros.Consensus.Protocol.Praos.Header ()
import Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Diffusion.Configuration
  ( PeerSharing (PeerSharingDisabled),
  )
import Ouroboros.Network.IOManager (withIOManager)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mux
  ( MiniProtocol (..),
    MiniProtocolLimits (..),
    MiniProtocolNum (MiniProtocolNum),
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
  )
import Ouroboros.Network.Protocol.ChainSync.Client
  ( ChainSyncClient (..),
  )
import Ouroboros.Network.Protocol.ChainSync.Client qualified as ChainSync
import Ouroboros.Network.Protocol.Handshake.Codec
  ( cborTermVersionDataCodec,
    noTimeLimitsHandshake,
    nodeToNodeHandshakeCodec,
  )
import Ouroboros.Network.Protocol.Handshake.Version
  ( Acceptable (acceptableVersion),
    Queryable (queryVersion),
    simpleSingletonVersions,
  )
import Ouroboros.Network.Snocket
  ( makeSocketBearer,
    socketSnocket,
  )
import Ouroboros.Network.Socket
  ( ConnectToArgs (..),
    HandshakeCallbacks (..),
    connectToNode,
    nullNetworkConnectTracers,
  )

-- | The application type for a chain sync client
type ChainSyncApplication = ChainSyncClient Header Point Tip IO ()

-- | The header hash type used in the chain sync connection
type HeaderHash = Network.HeaderHash Block

-- | Connect to a node-to-node chain sync server and run the given application
runChainSyncApplication ::
  -- | The network magic
  NetworkMagic ->
  -- | host
  String ->
  -- | port
  PortNumber ->
  -- | application
  (NodeToNodeVersionData -> ChainSyncApplication) ->
  IO (Either SomeException (Either () Void))
runChainSyncApplication magic peerName peerPort application = withIOManager $ \iocp -> do
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
        ctaConnectTracers = nullNetworkConnectTracers,
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
        (chainSyncToOuroboros . application) -- application
    )
    Nothing
    addrAddress

resolve :: String -> PortNumber -> IO AddrInfo
resolve peerName peerPort = do
  let hints =
        defaultHints
          { addrFlags = [AI_PASSIVE],
            addrSocketType = Stream
          }
  NE.head
    <$> getAddrInfo (Just hints) (Just peerName) (Just $ show peerPort)

-- TODO: provide sensible limits
-- https://github.com/intersectmbo/ouroboros-network/issues/575
maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
  MiniProtocolLimits
    { maximumIngressQueue = maxBound
    }

chainSyncToOuroboros ::
  -- | chainSync
  ChainSyncApplication ->
  OuroborosApplicationWithMinimalCtx
    Mx.InitiatorMode
    addr
    LazyByteString
    IO
    ()
    Void
chainSyncToOuroboros chainSyncApp =
  OuroborosApplication
    { getOuroborosApplication =
        [ MiniProtocol
            { miniProtocolNum = MiniProtocolNum 2,
              miniProtocolStart = StartOnDemand,
              miniProtocolLimits = maximumMiniProtocolLimits,
              miniProtocolRun = run
            }
        ]
    }
  where
    run = InitiatorProtocolOnly
        $ mkMiniProtocolCbFromPeer
        $ \_ctx ->
            ( nullTracer
            , codecChainSync
            , ChainSync.chainSyncClientPeer chainSyncApp
            )
