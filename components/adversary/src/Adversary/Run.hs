module Adversary.Run (run) where

import Adversary (Message (..), adversary)
import Adversary.SubmitTransactions (submitTxs)
import Control.Tracer (contramap, stdoutTracer)
import System.Environment (getArgs, getProgName)

run :: IO Message
run = do
  prog <- getProgName
  args <- getArgs
  case prog of
    "adversary" -> do
      adversary args
    "submit-txs" -> do
      submitTxs (contramap show stdoutTracer) args
    _ -> error $ "Unknown adversary program name: " <> prog
