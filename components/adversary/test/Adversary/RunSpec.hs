module Adversary.RunSpec where

import Adversary (Message (..))
import Adversary.Run (run)
import Control.Exception (ErrorCall)
import Data.Functor (void)
import System.Environment (withArgs, withProgName)
import Test.Hspec (Spec, it, shouldBe, shouldReturn, shouldThrow)

spec :: Spec
spec = do
  it "bails out with error given program name is 'foo'" $ do
    void (withArgs [] (withProgName "foo" run)) `shouldThrow` \(_ :: ErrorCall) -> True

  it "bails out with usage text given program name is 'adversary'" $ do
    withArgs [] (withProgName "adversary" run) `shouldReturn` Usage "Expected network-magic, port, sync-length, startPoint, number-of-connections and list-of-hosts arguments"

  it "returns Startup given program name is 'submit-txs'" $ do
    let args = ["arg1", "arg2"]
    withArgs args (withProgName "submit-txs" run) `shouldReturn` Usage "Usage: submit-txs <magic> <host> <port> <tx-file1> <tx-file2> ..."
