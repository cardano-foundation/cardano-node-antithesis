{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the pure 'Cardano.Antithesis.ForkTree' module.
module ForkTreeSpec (spec) where

import Cardano.Antithesis.ForkTree
    ( BlockHash
    , ForkTreeState
    , Tip (..)
    , clusterForkDepth
    , commonAncestor
    , currentTips
    , initialForkTreeState
    , recordExtension
    , setTip
    )
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import Test.Hspec

-- | Convenience: depth-cap large enough that nothing is truncated
-- in the unit tests below.
cap :: Int
cap = 1024

-- | Build a 'Tip' with derived slot number @= chainLength * 2@.
mkTip :: Text -> Int -> Tip
mkTip h len =
    Tip
        { tipHash = h
        , tipChainLength = len
        , tipSlotNo = len * 2
        }

-- | Sequentially extend a host's chain by @n@ blocks; each new
-- block's hash is @"<host><i>"@ for @i@ in @1..n@.
extendChain :: Text -> Int -> ForkTreeState -> ForkTreeState
extendChain host n acc =
    foldl
        (\s i -> recordExtension host (mkTip (host <> pack (show i)) i) s)
        acc
        [1 .. n]

spec :: Spec
spec = describe "ForkTree" $ do
    it "is empty initially" $ do
        let st = initialForkTreeState
        currentTips st `shouldBe` Map.empty
        commonAncestor cap st `shouldBe` (Nothing :: Maybe (BlockHash, Int))
        clusterForkDepth cap st `shouldBe` Nothing

    it "records sequential extension on a single host" $ do
        let st =
                recordExtension "p1" (mkTip "h3" 3)
                    . recordExtension "p1" (mkTip "h2" 2)
                    . recordExtension "p1" (mkTip "h1" 1)
                    $ initialForkTreeState
        Map.lookup "p1" (currentTips st)
            `shouldBe` Just (mkTip "h3" 3)
        commonAncestor cap st `shouldBe` Just ("h3", 3)
        clusterForkDepth cap st `shouldBe` Just 0

    it "two hosts at same tip have depth 0" $ do
        let st =
                recordExtension "p2" (mkTip "h2" 2)
                    . recordExtension "p2" (mkTip "h1" 1)
                    . recordExtension "p1" (mkTip "h2" 2)
                    . recordExtension "p1" (mkTip "h1" 1)
                    $ initialForkTreeState
        clusterForkDepth cap st `shouldBe` Just 0
        commonAncestor cap st `shouldBe` Just ("h2", 2)

    it "fork by 1 block: one host extended past the other" $ do
        let st =
                recordExtension "p2" (mkTip "h1" 1)
                    . recordExtension "p1" (mkTip "h2" 2)
                    . recordExtension "p1" (mkTip "h1" 1)
                    $ initialForkTreeState
        commonAncestor cap st `shouldBe` Just ("h1", 1)
        clusterForkDepth cap st `shouldBe` Just 1

    it "real fork: two distinct chains share only root block" $ do
        let st =
                recordExtension "p2" (mkTip "b2" 2)
                    . recordExtension "p2" (mkTip "b1" 1)
                    . recordExtension "p1" (mkTip "a2" 2)
                    . recordExtension "p1" (mkTip "a1" 1)
                    . recordExtension "p2" (mkTip "root" 0)
                    . recordExtension "p1" (mkTip "root" 0)
                    $ initialForkTreeState
        commonAncestor cap st `shouldBe` Just ("root", 0)
        clusterForkDepth cap st `shouldBe` Just 2

    it "deep divergence: 5-block fork" $ do
        let st0 =
                recordExtension "p2" (mkTip "root" 0)
                    . recordExtension "p1" (mkTip "root" 0)
                    $ initialForkTreeState
            st = extendChain "p2" 5 (extendChain "p1" 5 st0)
        commonAncestor cap st `shouldBe` Just ("root", 0)
        clusterForkDepth cap st `shouldBe` Just 5

    it "setTip without recordExtension does not record an edge" $ do
        let st =
                setTip "p2" (mkTip "h_alt" 1)
                    . recordExtension "p1" (mkTip "h2" 2)
                    . recordExtension "p1" (mkTip "h1" 1)
                    $ initialForkTreeState
        commonAncestor cap st `shouldBe` Nothing
        clusterForkDepth cap st `shouldBe` Nothing

    it "depth cap truncates ancestry walks" $ do
        let st0 =
                recordExtension "p2" (mkTip "root" 0)
                    . recordExtension "p1" (mkTip "root" 0)
                    $ initialForkTreeState
            st = extendChain "p2" 10 (extendChain "p1" 10 st0)
        -- Cap at 5: walks don't reach "root"; no common ancestor.
        commonAncestor 5 st `shouldBe` Nothing
        clusterForkDepth 5 st `shouldBe` Nothing
        -- Cap at 12: walks include "root"; common ancestor visible.
        commonAncestor 12 st `shouldBe` Just ("root", 0)
        clusterForkDepth 12 st `shouldBe` Just 10
