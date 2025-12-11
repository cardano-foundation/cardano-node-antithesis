module Adversary.Run (run) where

import Adversary (Message (..), adversary)
import Adversary.SubmitTransactions (submitTxs)
import Control.Tracer (contramap, stdoutTracer)
import System.Environment (getArgs)
import System.Exit (die)

run :: IO Message
run = do
  args <- getArgs
  case args of
    [] -> die usageMessage
    (cmd : cmdArgs) -> case cmd of
      "chainsync" -> adversary cmdArgs
      "submit" -> submitTxs (contramap show stdoutTracer) cmdArgs
      _ -> die $ "Unknown sub-command: " <> cmd <> "\n\n" <> usageMessage

usageMessage :: String
usageMessage =
  unlines
    [ "Usage: adversary <sub-command> [options]"
    , ""
    , "Sub-commands:"
    , "  chainsync <network-magic> <port> <sync-length> <chain-points-file> <num-connections> <host1> [host2...]"
    , "    Connect to Cardano nodes and sync blocks using ChainSync protocol"
    , ""
    , "  submit <network-magic> <host> <port> <tx-file1> [tx-file2...]"
    , "    Submit transactions by polling transaction files"
    , ""
    , "Examples:"
    , "  adversary chainsync 42 3001 100 points.json 5 node1.local node2.local"
    , "  adversary submit 42 node1.local 3001 tx1.hex tx2.hex"
    ]
