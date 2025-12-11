module Adversary.SubmitTransactions (submitTxs, SubmitOptions (..)) where

import Adversary (Message (..))
import Adversary.SubmitTransactions.Client (runTxSubmissionApplication)
import Adversary.SubmitTransactions.Log (SubmitLog (..))
import Adversary.SubmitTransactions.PollFiles (pollTransactionsFromFiles)
import Adversary.SubmitTransactions.Util (TxId')
import Cardano.Ledger.Alonzo.Tx ()
import Control.Concurrent.Async (cancel, link, withAsync)
import Control.Concurrent.Class.MonadSTM.Strict (newTBQueueIO)
import Control.Concurrent.Class.MonadSTM.Strict.TBQueue (StrictTBQueue)
import Control.Tracer (Tracer)
import Data.ByteString.Lazy (LazyByteString)
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Word (Word64)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseNodeToNode ()
import Ouroboros.Consensus.Shelley.Node.Serialisation ()
import Ouroboros.Network.Magic (NetworkMagic (..))

-- | Options for the Submit sub-command
data SubmitOptions = SubmitOptions
  { submitNetworkMagic :: Word64,
    submitHost :: String,
    submitPort :: Int,
    submitTxFiles :: NonEmpty FilePath
  }
  deriving (Show, Eq)

submitTxs :: Tracer IO SubmitLog -> SubmitOptions -> IO Message
submitTxs tracer SubmitOptions {..} = do
  let magic = NetworkMagic {unNetworkMagic = fromIntegral submitNetworkMagic}
  txsQueue :: StrictTBQueue IO (TxId', LazyByteString) <- newTBQueueIO 10
  withAsync (pollTransactionsFromFiles tracer (NE.toList submitTxFiles) txsQueue) $ \readerAsync -> do
    link readerAsync -- ensures exceptions are propagated
    void $
      runTxSubmissionApplication
        tracer
        magic
        submitHost
        (fromIntegral submitPort)
        txsQueue
    cancel readerAsync
    return Completed
