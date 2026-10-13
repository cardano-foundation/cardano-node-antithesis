{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the pure 'Cardano.Antithesis.ForkTree' module.
module ForkTreeSpec (spec) where

import Cardano.Antithesis.ForkTree
    ( ForkTreeState
    , Tip (..)
    , clusterForkDepth
    , currentTips
    , initialForkTreeState
    , parentOf
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
        clusterForkDepth cap st `shouldBe` Nothing

    it "records sequential extension on a single host" $ do
        let st =
                recordExtension "p1" (mkTip "h3" 3)
                    . recordExtension "p1" (mkTip "h2" 2)
                    . recordExtension "p1" (mkTip "h1" 1)
                    $ initialForkTreeState
        Map.lookup "p1" (currentTips st)
            `shouldBe` Just (mkTip "h3" 3)
        clusterForkDepth cap st `shouldBe` Just 0

    it "two hosts at same tip have depth 0" $ do
        let st =
                recordExtension "p2" (mkTip "h2" 2)
                    . recordExtension "p2" (mkTip "h1" 1)
                    . recordExtension "p1" (mkTip "h2" 2)
                    . recordExtension "p1" (mkTip "h1" 1)
                    $ initialForkTreeState
        clusterForkDepth cap st `shouldBe` Just 0

    it "a host one block behind is lagging, not forked" $ do
        let st =
                recordExtension "p2" (mkTip "h1" 1)
                    . recordExtension "p1" (mkTip "h2" 2)
                    . recordExtension "p1" (mkTip "h1" 1)
                    $ initialForkTreeState
        clusterForkDepth cap st `shouldBe` Just 0

    it "a host far behind on the same chain is lagging, not forked" $ do
        let st =
                recordExtension "p2" (mkTip "p13" 3)
                    . extendChain "p1" 600
                    $ initialForkTreeState
        clusterForkDepth cap st `shouldBe` Just 0

    it "real fork: two distinct chains share only root block" $ do
        let st =
                recordExtension "p2" (mkTip "b2" 2)
                    . recordExtension "p2" (mkTip "b1" 1)
                    . recordExtension "p1" (mkTip "a2" 2)
                    . recordExtension "p1" (mkTip "a1" 1)
                    . recordExtension "p2" (mkTip "root" 0)
                    . recordExtension "p1" (mkTip "root" 0)
                    $ initialForkTreeState
        clusterForkDepth cap st `shouldBe` Just 2

    it "deep divergence: 5-block fork" $ do
        let st0 =
                recordExtension "p2" (mkTip "root" 0)
                    . recordExtension "p1" (mkTip "root" 0)
                    $ initialForkTreeState
            st = extendChain "p2" 5 (extendChain "p1" 5 st0)
        clusterForkDepth cap st `shouldBe` Just 5

    it "a fork is measured from the shorter side even when the other lags" $ do
        let st0 =
                recordExtension "p2" (mkTip "root" 0)
                    . recordExtension "p1" (mkTip "root" 0)
                    $ initialForkTreeState
            st = extendChain "p2" 3 (extendChain "p1" 8 st0)
        clusterForkDepth cap st `shouldBe` Just 3

    it "a lagging host does not hide a fork between two others" $ do
        let st0 =
                recordExtension "p3" (mkTip "root" 0)
                    . recordExtension "p2" (mkTip "root" 0)
                    . recordExtension "p1" (mkTip "root" 0)
                    $ initialForkTreeState
            st =
                recordExtension "p3" (mkTip "p12" 2)
                    . extendChain "p2" 6
                    . extendChain "p1" 7
                    $ st0
        clusterForkDepth cap st `shouldBe` Just 6

    it "setTip without recordExtension does not record an edge" $ do
        let st =
                setTip "p2" (mkTip "h_alt" 1)
                    . recordExtension "p1" (mkTip "h2" 2)
                    . recordExtension "p1" (mkTip "h1" 1)
                    $ initialForkTreeState
        clusterForkDepth cap st `shouldBe` Nothing

    it "depth cap truncates ancestry walks" $ do
        let st0 =
                recordExtension "p2" (mkTip "root" 0)
                    . recordExtension "p1" (mkTip "root" 0)
                    $ initialForkTreeState
            st = extendChain "p2" 10 (extendChain "p1" 10 st0)
        -- Cap at 5: walks don't reach "root"; no placement possible.
        clusterForkDepth 5 st `shouldBe` Nothing
        -- Cap at 12: walks include "root"; the fork is visible.
        clusterForkDepth 12 st `shouldBe` Just 10

    it "a host that skips blocks keeps the parent edges others recorded" $ do
        let st =
                recordExtension "consumer" (mkTip "p19" 9)
                    . recordExtension "consumer" (mkTip "p13" 3)
                    . extendChain "p1" 9
                    $ initialForkTreeState
        parentOf "p19" st `shouldBe` Just ("p18", 8)
