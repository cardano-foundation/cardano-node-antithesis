{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}

module Adversary.FetchPeers.Client
  ( requestPeersFrom,
  )
where

import Cardano.Crypto.Libsodium (sodiumInit)
import Control.Concurrent.Class.MonadSTM.Strict (StrictTMVar, newEmptyTMVarIO, putTMVar, takeTMVar)
import Control.Monad.Class.MonadSTM (atomically)
import Control.Monad.Class.MonadThrow (Exception, throwIO)
import Control.Tracer (nullTracer)
import Data.ByteString.Lazy (LazyByteString)
import Data.Void (Void)
import Network.Mux qualified as Mx
import Network.Socket (SockAddr)
import Ouroboros.Network.Diffusion.Configuration (PeerSharing (..))
import Ouroboros.Network.IOManager (withIOManager)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mux
  ( MiniProtocol (..),
    MiniProtocolLimits (..),
    MiniProtocolNum (..),
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
  )
import Ouroboros.Network.PeerSelection.PeerSharing.Codec (decodeRemoteAddress, encodeRemoteAddress)
import Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec, nodeToNodeHandshakeCodec, noTimeLimitsHandshake)
import Ouroboros.Network.Protocol.Handshake.Version (Acceptable (acceptableVersion), Queryable (queryVersion), simpleSingletonVersions)
import Ouroboros.Network.Protocol.PeerSharing.Client (PeerSharingClient (..), peerSharingClientPeer)
import Ouroboros.Network.Protocol.PeerSharing.Codec (codecPeerSharing)
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount (..))
import Ouroboros.Network.Snocket (makeSocketBearer, socketSnocket)
import Ouroboros.Network.Socket (ConnectToArgs (..), HandshakeCallbacks (..), connectToNode, nullNetworkConnectTracers)

-- | PeerSharing mini-protocol number (from the spec)
peerSharingMiniProtocolNum :: MiniProtocolNum
peerSharingMiniProtocolNum = MiniProtocolNum 10

data PeerSharingError
  = -- | Received more peers than requested
    ProtocolViolation PeerSharingAmount Int
  deriving (Show)

instance Exception PeerSharingError

type PeerSharingApplication = PeerSharingClient SockAddr IO ()

-- | Request peers from a specific peer
requestPeersFrom ::
  -- | Network magic
  NetworkMagic ->
  -- | Peer address to request from
  SockAddr ->
  -- | Number of peers to request
  PeerSharingAmount ->
  IO [SockAddr]
requestPeersFrom magic peerAddr amount = do
  sodiumInit
  runPeerSharingApplication magic peerAddr (mkPeerSharingClient amount)

runPeerSharingApplication ::
  NetworkMagic ->
  SockAddr ->
  (StrictTMVar IO [SockAddr] -> PeerSharingApplication) ->
  IO [SockAddr]
runPeerSharingApplication magic peerAddr application = withIOManager $ \iocp -> do
  resultVar <- newEmptyTMVarIO

  _ <-
    connectToNode
      (socketSnocket iocp)
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
      mempty
      ( simpleSingletonVersions
          NodeToNodeV_14
          ( NodeToNodeVersionData
              { networkMagic = magic,
                diffusionMode = InitiatorOnlyDiffusionMode,
                peerSharing = PeerSharingEnabled,
                query = False
              }
          )
          (peerSharingToOuroboros . const (application resultVar))
      )
      Nothing
      peerAddr

  atomically $ takeTMVar resultVar

-- | Maximum mini protocol limits
maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
  MiniProtocolLimits
    { maximumIngressQueue = maxBound
    }

peerSharingToOuroboros ::
  PeerSharingApplication ->
  OuroborosApplicationWithMinimalCtx
    Mx.InitiatorMode
    addr
    LazyByteString
    IO
    ()
    Void
peerSharingToOuroboros peerSharingApp =
  OuroborosApplication
    { getOuroborosApplication =
        [ MiniProtocol
            { miniProtocolNum = peerSharingMiniProtocolNum,
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
          \_ctx ->
            ( nullTracer,
              codecPeerSharing (encodeRemoteAddress NodeToNodeV_14) (decodeRemoteAddress NodeToNodeV_14),
              peerSharingClientPeer peerSharingApp
            )

-- | Create a peer sharing client that requests peers and stores the result
mkPeerSharingClient ::
  PeerSharingAmount ->
  StrictTMVar IO [SockAddr] ->
  PeerSharingApplication
mkPeerSharingClient amount resultVar =
  SendMsgShareRequest amount $ \peers -> do
    -- Validate we didn't receive more than requested
    let numReceived = length peers
    if numReceived > fromIntegral amount
      then throwIO (ProtocolViolation amount numReceived)
      else do
        -- Store the result
        atomically $ putTMVar resultVar peers
        -- Terminate the protocol
        pure $ SendMsgDone (pure ())
