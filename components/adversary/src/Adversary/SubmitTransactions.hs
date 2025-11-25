module Adversary.SubmitTransactions (submitTxs) where

import Adversary (Message (..))

submitTxs :: [String] -> IO Message
submitTxs args = return $ Startup args
