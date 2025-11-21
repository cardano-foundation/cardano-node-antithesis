{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}

module Adversary.ChainSync.Connection
where

import Adversary.ChainSync.Codec
import Control.Exception (SomeException)
import Control.Tracer (Contravariant (contramap), stdoutTracer)
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty qualified as NE
import Data.Void (Void)
import Network.Mux qualified as Mx
import Network.Socket
    ( AddrInfo (..)
    , AddrInfoFlag (AI_PASSIVE)
    , PortNumber
    , SocketType (Stream)
    , defaultHints
    , getAddrInfo
    )
import Ouroboros.Consensus.Protocol.Praos.Header ()
import Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Diffusion.Configuration
    ( PeerSharing (PeerSharingDisabled)
    )
import Ouroboros.Network.IOManager (withIOManager)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mux
    ( MiniProtocol (..)
    , MiniProtocolLimits (..)
    , MiniProtocolNum (MiniProtocolNum)
    , OuroborosApplication (OuroborosApplication)
    , OuroborosApplicationWithMinimalCtx
    , RunMiniProtocol (InitiatorProtocolOnly)
    , RunMiniProtocolWithMinimalCtx
    , StartOnDemandOrEagerly (StartOnDemand)
    , mkMiniProtocolCbFromPeer
    )
import Ouroboros.Network.NodeToNode
    ( DiffusionMode (InitiatorOnlyDiffusionMode)
    , NodeToNodeVersion (NodeToNodeV_14)
    , NodeToNodeVersionData (..)
    , nodeToNodeCodecCBORTerm
    )
import Ouroboros.Network.Protocol.ChainSync.Client
    ( ChainSyncClient (..)
    )
import Ouroboros.Network.Protocol.ChainSync.Client qualified as ChainSync
import Ouroboros.Network.Protocol.Handshake.Codec
    ( cborTermVersionDataCodec
    , noTimeLimitsHandshake
    , nodeToNodeHandshakeCodec
    )
import Ouroboros.Network.Protocol.Handshake.Version
    ( Acceptable (acceptableVersion)
    , Queryable (queryVersion)
    , simpleSingletonVersions
    )
import Ouroboros.Network.Snocket
    ( makeSocketBearer
    , socketSnocket
    )
import Ouroboros.Network.Socket
    ( ConnectToArgs (..)
    , HandshakeCallbacks (..)
    , connectToNode
    , nullNetworkConnectTracers
    )

type HeaderHash = Network.HeaderHash Block

clientChainSync
    :: NetworkMagic
    -> String -- host
    -> PortNumber -- port
    -> (NodeToNodeVersionData -> ChainSyncClient Header Point Tip IO ())
    -> IO (Either SomeException (Either () Void))
clientChainSync magic peerName peerPort application = withIOManager $ \iocp -> do
    AddrInfo{addrAddress} <- resolve peerName peerPort
    connectToNode -- withNode
        (socketSnocket iocp) -- TCP
        makeSocketBearer
        ConnectToArgs
            { ctaHandshakeCodec = nodeToNodeHandshakeCodec
            , ctaHandshakeTimeLimits = noTimeLimitsHandshake
            , ctaVersionDataCodec =
                cborTermVersionDataCodec
                    nodeToNodeCodecCBORTerm
            , ctaConnectTracers = nullNetworkConnectTracers
            , ctaHandshakeCallbacks =
                HandshakeCallbacks
                    { acceptCb = acceptableVersion
                    , queryCb = queryVersion
                    }
            }
        mempty -- socket options
        ( simpleSingletonVersions
            NodeToNodeV_14
            ( NodeToNodeVersionData
                { networkMagic = magic
                , diffusionMode = InitiatorOnlyDiffusionMode
                , peerSharing = PeerSharingDisabled
                , query = False
                }
            )
            (app . application) -- application
        )
        Nothing
        addrAddress

resolve :: String -> PortNumber -> IO AddrInfo
resolve peerName peerPort = do
    let hints =
            defaultHints
                { addrFlags = [AI_PASSIVE]
                , addrSocketType = Stream
                }
    NE.head
        <$> getAddrInfo (Just hints) (Just peerName) (Just $ show peerPort)

app
    :: ChainSyncClient Header Point Tip IO ()
    -> OuroborosApplicationWithMinimalCtx
        Mx.InitiatorMode
        addr
        LBS.ByteString
        IO
        ()
        Void
app client = miniProtocolToOuroborosApplication
    $ InitiatorProtocolOnly
    $ mkMiniProtocolCbFromPeer
    $ \_ctx ->
        ( contramap show stdoutTracer -- tracer
        , codecChainSync
        , ChainSync.chainSyncClientPeer client
        )

-- TODO: provide sensible limits
-- https://github.com/intersectmbo/ouroboros-network/issues/575
maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
    MiniProtocolLimits
        { maximumIngressQueue = maxBound
        }

--
-- Chain sync demo
--

miniProtocolToOuroborosApplication
    :: RunMiniProtocolWithMinimalCtx appType addr bytes m a b
    -- ^ chainSync
    -> OuroborosApplicationWithMinimalCtx appType addr bytes m a b
miniProtocolToOuroborosApplication chainSync =
    OuroborosApplication
        [ MiniProtocol
            { miniProtocolNum = MiniProtocolNum 2
            , miniProtocolStart = StartOnDemand
            , miniProtocolLimits = maximumMiniProtocolLimits
            , miniProtocolRun = chainSync
            }
        ]
