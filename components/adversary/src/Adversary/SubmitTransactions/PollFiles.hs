module Adversary.SubmitTransactions.PollFiles where

import Adversary.SubmitTransactions.Log (SubmitLog (..))
import Adversary.SubmitTransactions.Util (TxId', mkTxId, unsafeFromHex)
import Cardano.Ledger.Alonzo.Tx ()
import Control.Concurrent (threadDelay)
import Control.Concurrent.Class.MonadSTM.Strict (atomically, writeTBQueue)
import Control.Concurrent.Class.MonadSTM.Strict.TBQueue (StrictTBQueue)
import Control.Monad (forM_, forever)
import Control.Tracer (Tracer, traceWith)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseNodeToNode ()
import Ouroboros.Consensus.Shelley.Node.Serialisation ()
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FSNotify (Event (Added, Modified), watchTree, withManager)

-- | Poll the given files and directories for new or modified transaction files.
pollTransactionsFromFiles :: Tracer IO SubmitLog -> [String] -> StrictTBQueue IO (TxId', LazyByteString) -> IO ()
pollTransactionsFromFiles tracer files txQueue =
  withManager $ \mgr -> do
    forM_ files $ \file -> do
      isDir <- doesDirectoryExist file
      if isDir
        then
          watchTree
            mgr
            file
            (const True)
            ( \case
                Added path _ _ -> readAndEnqueue path
                Modified path _ _ -> readAndEnqueue path
                other -> traceWith tracer (IgnoringFSEvent other)
            )
            >> traceWith tracer (WatchingDirectory file)
        else readAndEnqueue file
    forever (threadDelay 1000000)
  where
    readAndEnqueue path = do
      isFile <- doesFileExist path
      if isFile
        then do
          traceWith tracer (ReadingTxFile path)
          hexContents <- LBS.readFile path
          let txBytes = unsafeFromHex hexContents
          case mkTxId txBytes of
            Left err -> traceWith tracer (FailedToComputeTxId path err)
            Right txid -> do
              traceWith tracer (EnqueuingTx path txid)
              atomically $ writeTBQueue txQueue (txid, txBytes)
        else traceWith tracer (FileDoesNotExist path)
