module Adversary.SubmitTransactions where

import Adversary (Message (..), readOrFail)
import Adversary.SubmitTransactions.Client (runTxSubmissionApplication)
import Adversary.SubmitTransactions.Log (SubmitLog (..))
import Adversary.SubmitTransactions.PollFiles (pollTransactionsFromFiles)
import Adversary.SubmitTransactions.Util (TxId')
import Cardano.Ledger.Alonzo.Tx ()
import Control.Concurrent.Async (cancel, link, withAsync)
import Control.Concurrent.Class.MonadSTM.Strict (newTBQueueIO)
import Control.Concurrent.Class.MonadSTM.Strict.TBQueue (StrictTBQueue)
import Control.Tracer (Tracer, traceWith)
import Data.ByteString.Lazy (LazyByteString)
import Data.Functor (void)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseNodeToNode ()
import Ouroboros.Consensus.Shelley.Node.Serialisation ()
import Ouroboros.Network.Magic (NetworkMagic (..))

submitTxs :: Tracer IO SubmitLog -> [String] -> IO Message
submitTxs tracer = \case
  args@(magicArg : host : port : hexEncodedTxFiles) -> do
    traceWith tracer $ Starting args
    let magic = NetworkMagic {unNetworkMagic = readOrFail "magic" magicArg}
    txsQueue :: StrictTBQueue IO (TxId', LazyByteString) <- newTBQueueIO 10
    withAsync (pollTransactionsFromFiles tracer hexEncodedTxFiles txsQueue) $ \readerAsync -> do
      link readerAsync -- ensures exceptions are propagated
      void $
        runTxSubmissionApplication
          tracer
          magic
          host
          (readOrFail "port" port)
          txsQueue
      cancel readerAsync
      return Completed
  _ -> pure $ Usage "Usage: submit-txs <magic> <host> <port> <tx-file1> <tx-file2> ..."
