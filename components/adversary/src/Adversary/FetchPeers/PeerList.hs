{-# LANGUAGE OverloadedStrings #-}

module Adversary.FetchPeers.PeerList
  ( PeerList,
    empty,
    singleton,
    addPeers,
    samplePeers,
    size,
    toList,
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import Network.Socket (SockAddr)
import System.Random (RandomGen, uniformR)

-- | A set of known peers
newtype PeerList = PeerList {unPeerList :: Set SockAddr}
  deriving (Show, Eq)

-- | Create an empty peer list
empty :: PeerList
empty = PeerList Set.empty

-- | Create a peer list with a single peer
singleton :: SockAddr -> PeerList
singleton addr = PeerList (Set.singleton addr)

-- | Add multiple peers to the list
addPeers :: [SockAddr] -> PeerList -> PeerList
addPeers peers (PeerList peerSet) =
  PeerList (Set.union peerSet (Set.fromList peers))

-- | Sample N random peers from the list
samplePeers :: RandomGen g => Int -> PeerList -> g -> ([SockAddr], g)
samplePeers n (PeerList peerSet) gen
  | Set.null peerSet = ([], gen)
  | n <= 0 = ([], gen)
  | otherwise = go n (Set.toList peerSet) [] gen
  where
    go 0 _ acc g = (acc, g)
    go _ [] acc g = (acc, g)
    go remaining peers acc g =
      let (idx, g') = uniformR (0, length peers - 1) g
          peer = peers !! idx
          newPeers = take idx peers ++ drop (idx + 1) peers
       in go (remaining - 1) newPeers (peer : acc) g'

-- | Get the size of the peer list
size :: PeerList -> Int
size (PeerList peerSet) = Set.size peerSet

-- | Convert peer list to a list
toList :: PeerList -> [SockAddr]
toList (PeerList peerSet) = Set.toList peerSet
