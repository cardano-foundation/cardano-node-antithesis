module Adversary.Run (run) where

import Adversary (Message (..), adversary)
import Adversary.SubmitTransactions (submitTxs)
import System.Environment (getArgs, getProgName)

run :: IO Message
run = do
  prog <- getProgName
  args <- getArgs
  case prog of
    "adversary" -> do
      adversary args
    "submit-txs" -> do
      submitTxs args
    _ -> error $ "Unknown adversary program name: " <> prog
