module Adversary.SubmitTransactions.Log where

import Adversary.SubmitTransactions.Util (TxId')
import Cardano.Ledger.Alonzo.Tx ()
import Data.ByteString.Lazy (LazyByteString)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseNodeToNode ()
import Ouroboros.Consensus.Shelley.Node.Serialisation ()
import Ouroboros.Network.NodeToNode
  ( TraceSendRecv,
  )
import Ouroboros.Network.Protocol.TxSubmission2.Type (TxSubmission2)
import System.FSNotify (Event)

data SubmitLog
  = Starting {args :: [String]}
  | IgnoringFSEvent {fsEvent :: Event}
  | WatchingDirectory {directory :: FilePath}
  | ReadingTxFile {txFilePath :: FilePath}
  | FailedToComputeTxId {txFilePath :: FilePath, errorMsg :: String}
  | EnqueuingTx {txFilePath :: FilePath, txid :: TxId'}
  | FileDoesNotExist {path :: FilePath}
  | ReceivedRequestTxIds {numToAck :: Word, numToReq :: Word}
  | SendingTxIds {txid :: TxId'}
  | ReceivedRequestTxs {reqTxIds :: [TxId']}
  | SendingRequestedTxs {numTxs :: Int}
  | NetworkLog {message :: TraceSendRecv (TxSubmission2 TxId' LazyByteString)}
  deriving (Show)
