module Adversary.SubmitTransactions (submitTxs, SubmitOptions (..)) where

import Adversary (Message (..))
import Adversary.FetchPeers.Database (closeDatabase, loadPeers, openDatabase, PeerRecord (..))
import Adversary.SubmitTransactions.Client (runTxSubmissionApplication)
import Adversary.SubmitTransactions.Log (SubmitLog (..))
import Adversary.SubmitTransactions.PollFiles (pollTransactionsFromFiles)
import Adversary.SubmitTransactions.Util (TxId')
import Cardano.Ledger.Alonzo.Tx ()
import Control.Concurrent.Async (cancel, link, mapConcurrently_, withAsync)
import Control.Concurrent.Class.MonadSTM.Strict (newTBQueueIO)
import Control.Concurrent.Class.MonadSTM.Strict.TBQueue (StrictTBQueue)
import Control.Exception (bracket)
import Control.Tracer (Tracer, traceWith)
import Data.ByteString.Lazy (LazyByteString)
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Word (Word64)
import Network.Socket (HostAddress, PortNumber, SockAddr (..))
import Data.Word (Word8)
import Data.Bits ((.&.), shiftR)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseNodeToNode ()
import Ouroboros.Consensus.Shelley.Node.Serialisation ()
import Ouroboros.Network.Magic (NetworkMagic (..))
import System.Random (randomRIO)

-- | Options for the Submit sub-command
data SubmitOptions = SubmitOptions
  { submitNetworkMagic :: Word64,
    submitHost :: Maybe String,
    submitPort :: Maybe Int,
    submitPeersDatabase :: Maybe FilePath,
    submitTxFiles :: NonEmpty FilePath
  }
  deriving (Show, Eq)

submitTxs :: Tracer IO SubmitLog -> SubmitOptions -> IO Message
submitTxs tracer SubmitOptions {..} = do
  let magic = NetworkMagic {unNetworkMagic = fromIntegral submitNetworkMagic}

  -- Validate that at least one peer source is provided
  case (submitHost, submitPort, submitPeersDatabase) of
    (Nothing, Nothing, Nothing) ->
      error "At least one of --host/--port or --peers-database must be provided"
    (Just _, Nothing, _) ->
      error "If --host is provided, --port must also be provided"
    (Nothing, Just _, _) ->
      error "If --port is provided, --host must also be provided"
    _ -> pure ()

  -- Build list of peers to connect to
  peersToConnect <- buildPeerList submitHost submitPort submitPeersDatabase

  traceWith tracer $ ConnectingToPeers (length peersToConnect)

  -- Create shared queue for transactions
  txsQueue :: StrictTBQueue IO (TxId', LazyByteString) <- newTBQueueIO 10

  -- Start file polling in the background
  withAsync (pollTransactionsFromFiles tracer (NE.toList submitTxFiles) txsQueue) $ \readerAsync -> do
    link readerAsync -- ensures exceptions are propagated

    -- Connect to all peers concurrently
    mapConcurrently_
      (\(host, port) -> void $ runTxSubmissionApplication tracer magic host port txsQueue)
      peersToConnect

    cancel readerAsync
    return Completed

-- | Build the list of (host, port) pairs to connect to
buildPeerList :: Maybe String -> Maybe Int -> Maybe FilePath -> IO [(String, PortNumber)]
buildPeerList maybeHost maybePort maybePeersDb = do
  -- Get peer from database if provided
  dbPeer <- case maybePeersDb of
    Nothing -> pure Nothing
    Just dbPath -> selectRandomPeerFromDatabase dbPath

  -- Get provided host:port if available
  let providedPeer = case (maybeHost, maybePort) of
        (Just host, Just port) -> Just (host, fromIntegral port)
        _ -> Nothing

  -- Combine both sources
  case (providedPeer, dbPeer) of
    (Nothing, Nothing) -> error "No peers available"
    (Just peer, Nothing) -> pure [peer]
    (Nothing, Just peer) -> pure [peer]
    (Just peer1, Just peer2) -> pure [peer1, peer2]

-- | Select a random peer from the database
selectRandomPeerFromDatabase :: FilePath -> IO (Maybe (String, PortNumber))
selectRandomPeerFromDatabase dbPath = do
  bracket (openDatabase dbPath) closeDatabase $ \db -> do
    peers <- loadPeers db
    case peers of
      [] -> pure Nothing
      _ -> do
        idx <- randomRIO (0, length peers - 1)
        let peer = address (peers !! idx)
        pure $ Just (sockAddrToHostPort peer)

-- | Convert SockAddr to (host, port) pair
sockAddrToHostPort :: SockAddr -> (String, PortNumber)
sockAddrToHostPort (SockAddrInet port host) =
  (hostAddressToString host, port)
sockAddrToHostPort (SockAddrInet6 port _flow (w1, w2, w3, w4) _scope) =
  (show w1 ++ ":" ++ show w2 ++ ":" ++ show w3 ++ ":" ++ show w4, port)
sockAddrToHostPort (SockAddrUnix path) =
  error $ "Unix sockets not supported for tx submission: " ++ path

-- | Convert HostAddress (Word32) to dotted-quad string
-- HostAddress is in network byte order (big-endian)
hostAddressToString :: HostAddress -> String
hostAddressToString addr =
  let byte1 = fromIntegral (addr .&. 0xFF) :: Word8
      byte2 = fromIntegral ((addr `shiftR` 8) .&. 0xFF) :: Word8
      byte3 = fromIntegral ((addr `shiftR` 16) .&. 0xFF) :: Word8
      byte4 = fromIntegral ((addr `shiftR` 24) .&. 0xFF) :: Word8
   in show byte1 ++ "." ++ show byte2 ++ "." ++ show byte3 ++ "." ++ show byte4
