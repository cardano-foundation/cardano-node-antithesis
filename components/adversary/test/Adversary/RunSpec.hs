module Adversary.RunSpec where

import Adversary (ChainSyncOptions (..))
import Adversary.Run (Command (..))
import Adversary.SubmitTransactions (SubmitOptions (..))
import Data.List.NonEmpty qualified as NE
import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

-- Import the parser from Adversary.Run
import Adversary.Run qualified as Run

spec :: Spec
spec = do
  describe "Command-line parsing" $ do
    it "fails when no arguments provided" $ do
      let result = execParserPure defaultPrefs Run.programInfo []
      result `shouldSatisfy` \case
        Failure _ -> True
        _ -> False

    it "fails with unknown sub-command" $ do
      let result = execParserPure defaultPrefs Run.programInfo ["unknown-cmd"]
      result `shouldSatisfy` \case
        Failure _ -> True
        _ -> False

    it "fails when chainsync sub-command has missing required options" $ do
      let result = execParserPure defaultPrefs Run.programInfo ["chainsync"]
      result `shouldSatisfy` \case
        Failure _ -> True
        _ -> False

    it "fails when submit sub-command has missing required options" $ do
      let result = execParserPure defaultPrefs Run.programInfo ["submit"]
      result `shouldSatisfy` \case
        Failure _ -> True
        _ -> False

    it "parses valid chainsync command with long options" $ do
      let args =
            [ "chainsync"
            , "--network-magic"
            , "42"
            , "--port"
            , "3001"
            , "--sync-length"
            , "100"
            , "--chain-points-file"
            , "points.json"
            , "--num-connections"
            , "5"
            , "node1"
            , "node2"
            ]
      let result = execParserPure defaultPrefs Run.programInfo args
      case result of
        Success (ChainSyncCommand opts) -> do
          csNetworkMagic opts `shouldBe` 42
          csPort opts `shouldBe` 3001
          csSyncLength opts `shouldBe` 100
          csChainPointsFile opts `shouldBe` "points.json"
          csNumConnections opts `shouldBe` 5
          NE.toList (csHosts opts) `shouldBe` ["node1", "node2"]
        _ -> fail "Expected successful parse of ChainSyncCommand"

    it "parses valid chainsync command with short options" $ do
      let args =
            [ "chainsync"
            , "-m"
            , "42"
            , "-p"
            , "3001"
            , "-l"
            , "100"
            , "-f"
            , "points.json"
            , "-n"
            , "5"
            , "node1"
            ]
      let result = execParserPure defaultPrefs Run.programInfo args
      case result of
        Success (ChainSyncCommand opts) -> do
          csNetworkMagic opts `shouldBe` 42
          csPort opts `shouldBe` 3001
          csSyncLength opts `shouldBe` 100
          csChainPointsFile opts `shouldBe` "points.json"
          csNumConnections opts `shouldBe` 5
          NE.toList (csHosts opts) `shouldBe` ["node1"]
        _ -> fail "Expected successful parse of ChainSyncCommand"

    it "parses valid submit command with long options" $ do
      let args =
            [ "submit"
            , "--network-magic"
            , "42"
            , "--host"
            , "node1"
            , "--port"
            , "3001"
            , "tx1.hex"
            , "tx2.hex"
            ]
      let result = execParserPure defaultPrefs Run.programInfo args
      case result of
        Success (SubmitCommand opts) -> do
          submitNetworkMagic opts `shouldBe` 42
          submitHost opts `shouldBe` "node1"
          submitPort opts `shouldBe` 3001
          NE.toList (submitTxFiles opts) `shouldBe` ["tx1.hex", "tx2.hex"]
        _ -> fail "Expected successful parse of SubmitCommand"

    it "parses valid submit command with short options" $ do
      let args =
            [ "submit"
            , "-m"
            , "764824073"
            , "-h"
            , "mainnet.node"
            , "-p"
            , "30000"
            , "tx1.hex"
            ]
      let result = execParserPure defaultPrefs Run.programInfo args
      case result of
        Success (SubmitCommand opts) -> do
          submitNetworkMagic opts `shouldBe` 764824073
          submitHost opts `shouldBe` "mainnet.node"
          submitPort opts `shouldBe` 30000
          NE.toList (submitTxFiles opts) `shouldBe` ["tx1.hex"]
        _ -> fail "Expected successful parse of SubmitCommand"
