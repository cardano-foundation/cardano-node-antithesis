{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Adversary.FetchPeers (fetchPeers, FetchPeersOptions (..)) where

import Adversary (Message (..), readOrFail)
import Adversary.FetchPeers.Client (requestPeersFrom)
import Adversary.FetchPeers.Database (PeerDatabase, PeerRecord (..), closeDatabase, loadPeers, openDatabase, savePeers)
import Adversary.FetchPeers.Log (FetchLog (..))
import Adversary.FetchPeers.PeerList (PeerList)
import Adversary.FetchPeers.PeerList qualified as PeerList
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, bracket, catch, displayException)
import Control.Tracer (Tracer, traceWith)
import Data.Word (Word64)
import Network.Socket (AddrInfo (..), PortNumber, SockAddr, defaultHints, getAddrInfo)
import Network.Socket qualified as Socket
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount (..))
import System.Random (newStdGen)

-- | Options for the FetchPeers command
data FetchPeersOptions = FetchPeersOptions
  { fpNetworkMagic :: Word64,
    fpInitialPeer :: String,
    fpPollInterval :: Int,
    fpNumPeers :: Int,
    fpDatabasePath :: FilePath
  }
  deriving (Show, Eq)

-- | Entry point for the fetch-peers command
fetchPeers :: Tracer IO FetchLog -> FetchPeersOptions -> IO Message
fetchPeers tracer FetchPeersOptions {..} = do
  let magic = NetworkMagic {unNetworkMagic = fromIntegral fpNetworkMagic}

  -- Parse initial peer
  initialAddr <- parseHostPort fpInitialPeer

  traceWith tracer $ Configuration initialAddr fpPollInterval fpNumPeers fpDatabasePath

  -- Open database and run the fetch loop
  bracket (openDatabase fpDatabasePath) closeDatabase $ \db -> do
    -- Load existing peers from database
    existingPeers <- loadPeers db
    traceWith tracer $ LoadedPeersFromDatabase (length existingPeers)

    -- Build initial peer list from database + initial peer
    let dbPeers = map address existingPeers
    let initialPeerList =
          if null dbPeers
            then PeerList.singleton initialAddr
            else PeerList.addPeers (initialAddr : dbPeers) PeerList.empty

    traceWith tracer $ StartingWithPeers (PeerList.size initialPeerList)

    -- Run the fetch loop with database
    runFetchLoop tracer db magic fpPollInterval (PeerSharingAmount $ fromIntegral fpNumPeers) initialPeerList

  pure Completed

-- | Run the main fetch loop with database persistence
runFetchLoop ::
  Tracer IO FetchLog ->
  PeerDatabase ->
  NetworkMagic ->
  Int ->
  PeerSharingAmount ->
  PeerList ->
  IO ()
runFetchLoop tracer db magic pollInterval numPeers initialPeerList = do
  gen <- newStdGen
  loop gen initialPeerList
  where
    loop gen peerList = do
      traceWith tracer BeginFetchIteration
      traceWith tracer $ KnownPeers (PeerList.size peerList)

      -- Sample some random peers to request from
      let numToSample = min 10 (PeerList.size peerList) -- Sample up to 3 peers
      let (sampled, gen') = PeerList.samplePeers numToSample peerList gen

      traceWith tracer $ SampledPeers (length sampled)

      -- Request peers from each sampled peer (concurrently)
      newPeers <- concat <$> mapConcurrently (\peer -> requestPeersFromPeer tracer magic peer numPeers) sampled

      traceWith tracer $ ReceivedTotalPeers (length newPeers)

      -- Update peer list
      let updatedPeerList = PeerList.addPeers newPeers peerList

      -- Save new peers to database
      if not (null newPeers)
        then do
          traceWith tracer $ SavingPeersToDatabase (length newPeers)
          savePeers db newPeers
        else traceWith tracer NoNewPeersToSave

      traceWith tracer $ UpdatedPeerList (PeerList.size updatedPeerList)

      -- Wait before next iteration
      traceWith tracer $ SleepingBeforeNextIteration pollInterval
      threadDelay (pollInterval * 1_000_000)

      -- Continue loop
      loop gen' updatedPeerList

-- | Request peers from a single peer, with error handling
requestPeersFromPeer ::
  Tracer IO FetchLog ->
  NetworkMagic ->
  SockAddr ->
  PeerSharingAmount ->
  IO [SockAddr]
requestPeersFromPeer tracer magic peer amount =
  catch
    (do
        traceWith tracer $ RequestingFromPeer peer
        peers <- requestPeersFrom tracer magic peer amount
        traceWith tracer $ ReceivedPeersFromPeer peer (length peers)
        pure peers
    )
    (\(e :: SomeException) -> do
        traceWith tracer $ ErrorRequestingFromPeer peer (displayException e)
        pure []
    )

-- | Parse a "host:port" string into a SockAddr
parseHostPort :: String -> IO SockAddr
parseHostPort hostPort = case break (== ':') hostPort of
  (host, ':' : portStr) -> do
    let port = readOrFail "port" portStr :: PortNumber
    resolve host port
  _ -> error $ "Invalid host:port format: " ++ hostPort

-- | Resolve a hostname and port to a SockAddr
resolve :: String -> PortNumber -> IO SockAddr
resolve host port = do
  let hints =
        defaultHints
          { Socket.addrFlags = [Socket.AI_PASSIVE],
            Socket.addrSocketType = Socket.Stream
          }
  addrs <- getAddrInfo (Just hints) (Just host) (Just $ show port)
  case addrs of
    [] -> error $ "Could not resolve: " ++ host ++ ":" ++ show port
    (addr : _) -> pure (addrAddress addr)
