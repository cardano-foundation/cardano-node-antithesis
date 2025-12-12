module Adversary.SubmitTransactions.Log where

import Adversary.SubmitTransactions.Util (TxId')
import Cardano.Ledger.Alonzo.Tx ()
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseNodeToNode ()
import Ouroboros.Consensus.Shelley.Node.Serialisation ()
import System.FSNotify (Event)

data SubmitLog
  = IgnoringFSEvent {fsEvent :: Event}
  | WatchingDirectory {directory :: FilePath}
  | ReadingTxFile {txFilePath :: FilePath}
  | FailedToComputeTxId {txFilePath :: FilePath, errorMsg :: String}
  | EnqueuingTx {txFilePath :: FilePath, txid :: TxId'}
  | FileDoesNotExist {path :: FilePath}
  | ConnectingToPeers {numPeers :: Int}
  | ReceivedRequestTxIds {numToAck :: Word, numToReq :: Word}
  | SendingTxIds {blocking:: Bool, txid :: TxId'}
  | ReceivedRequestTxs {reqTxIds :: [TxId']}
  | SendingRequestedTxs {numTxs :: Int}
  | NetworkLog {message :: String}
  deriving (Show)
