module Adversary.FetchPeers.Log where

import Network.Socket (SockAddr)

data FetchLog
  = Configuration {peerAddr :: SockAddr, pollInterval :: Int, numPeersPerRequest :: Int, dbPath :: FilePath}
  | LoadedPeersFromDatabase {numPeers :: Int}
  | StartingWithPeers {totalPeers :: Int}
  | BeginFetchIteration
  | KnownPeers {numKnown :: Int}
  | SampledPeers {numSampled :: Int}
  | RequestingFromPeer {peerAddr :: SockAddr}
  | ReceivedPeersFromPeer {peerAddr :: SockAddr, numReceived :: Int}
  | ErrorRequestingFromPeer {peerAddr :: SockAddr, errorMsg :: String}
  | ReceivedTotalPeers {totalReceived :: Int}
  | SavingPeersToDatabase {numToSave :: Int}
  | NoNewPeersToSave
  | UpdatedPeerList {newTotal :: Int}
  | SleepingBeforeNextIteration {seconds :: Int}
  deriving (Show)
