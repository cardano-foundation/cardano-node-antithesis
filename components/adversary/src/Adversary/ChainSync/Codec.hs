module Adversary.ChainSync.Codec
    ( codecChainSync
    , Block
    , Header
    , Tip
    , Point
    ) where

import Cardano.Chain.Slotting (EpochSlots (EpochSlots))
import Codec.Serialise qualified as CBOR
import Codec.Serialise.Decoding qualified as CBOR
import Codec.Serialise.Encoding qualified as CBOR
import Data.ByteString.Lazy qualified as LBS
import Data.Data (Proxy (Proxy))
import Network.TypedProtocol.Codec (Codec)
import Ouroboros.Consensus.Block.Abstract
    ( decodeRawHash
    , encodeRawHash
    )
import Ouroboros.Consensus.Byron.Ledger (ByronBlock, CodecConfig (..))
import Ouroboros.Consensus.Cardano.Block
    ( CodecConfig (CardanoCodecConfig)
    )
import Ouroboros.Consensus.Cardano.Block qualified as Consensus
import Ouroboros.Consensus.Cardano.Node
    ( pattern CardanoNodeToNodeVersion2
    )
import Ouroboros.Consensus.HardFork.Combinator.NetworkVersion
    ( HardForkNodeToNodeVersion
    )
import Ouroboros.Consensus.Node.Serialisation
    ( decodeNodeToNode
    , encodeNodeToNode
    )
import Ouroboros.Consensus.Protocol.Praos.Header ()
import Ouroboros.Consensus.Shelley.Ledger
    ( CodecConfig (ShelleyCodecConfig)
    )
import Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block
    ( decodeTip
    , encodeTip
    )
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Protocol.ChainSync.Codec qualified as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Type qualified as ChainSync

type Block = Consensus.CardanoBlock Consensus.StandardCrypto

type Header = Consensus.Header Block

type Tip = Network.Tip Block

type Point = Network.Point Header

codecChainSync
    :: Codec
        (ChainSync.ChainSync Header Point Tip)
        CBOR.DeserialiseFailure
        IO
        LBS.ByteString
codecChainSync =
    ChainSync.codecChainSync
        encHeader
        decHeader
        encPoint
        decPoint
        encTip
        decTip

----- Encoding and Decoding Headers -----
encHeader :: Header -> CBOR.Encoding
encHeader = encodeNodeToNode @Block ccfg version

decHeader :: CBOR.Decoder s Header
decHeader = decodeNodeToNode @Block ccfg version

version
    :: HardForkNodeToNodeVersion
        (ByronBlock : Consensus.CardanoShelleyEras c)
version = CardanoNodeToNodeVersion2

ccfg :: Consensus.CardanoCodecConfig c
ccfg =
    CardanoCodecConfig
        (ByronCodecConfig $ EpochSlots 42)
        ShelleyCodecConfig
        ShelleyCodecConfig
        ShelleyCodecConfig
        ShelleyCodecConfig
        ShelleyCodecConfig
        ShelleyCodecConfig

--- Encoding and Decoding Points -----
encPoint :: Point -> CBOR.Encoding
encPoint = CBOR.encode
decPoint :: CBOR.Decoder s Point
decPoint = CBOR.decode

--- Encoding and Decoding Tips -----
encTip :: Network.Tip Block -> CBOR.Encoding
encTip = encodeTip (encodeRawHash (Proxy @Block))
decTip :: CBOR.Decoder s (Network.Tip Block)
decTip = decodeTip (decodeRawHash (Proxy @Block))
