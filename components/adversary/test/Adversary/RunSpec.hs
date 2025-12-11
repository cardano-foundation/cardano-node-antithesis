module Adversary.RunSpec where

import Adversary (Message (..))
import Adversary.Run (run)
import Data.Functor (void)
import System.Environment (withArgs)
import System.Exit (ExitCode (..))
import Test.Hspec (Spec, it, shouldReturn, shouldThrow)

spec :: Spec
spec = do
  it "exits with error when no arguments provided" $ do
    void (withArgs [] run) `shouldThrow` \(_ :: ExitCode) -> True

  it "exits with error given unknown sub-command" $ do
    void (withArgs ["unknown-cmd"] run) `shouldThrow` \(_ :: ExitCode) -> True

  it "bails out with usage text given 'chainsync' sub-command with no arguments" $ do
    withArgs ["chainsync"] run `shouldReturn` Usage "Expected network-magic, port, sync-length, startPoint, number-of-connections and list-of-hosts arguments"

  it "returns Usage given 'submit' sub-command with insufficient arguments" $ do
    withArgs ["submit", "arg1", "arg2"] run `shouldReturn` Usage "Usage: submit-txs <magic> <host> <port> <tx-file1> <tx-file2> ..."
