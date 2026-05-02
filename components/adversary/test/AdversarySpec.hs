{-# OPTIONS_GHC -Wno-orphans #-}

module AdversarySpec where

import Adversary
    ( ChainPointSamples (..)
    , Point
    , originPoint
    , pickOne
    , readChainPoint
    , showChainPoint
    )
import Adversary.Application (Limit (..))
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust, fromMaybe)
import System.Random (mkStdGen)
import Test.Hspec
    ( Spec
    , it
    , shouldBe
    , shouldContain
    , shouldNotBe
    )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
    ( Arbitrary (arbitrary)
    , Positive (..)
    , checkCoverage
    , cover
    , elements
    , listOf
    , property
    )

spec :: Spec
spec = do
    it "Reads Limit"
        $ read "20"
        `shouldBe` Limit 20

    it "readChainPoint can read point"
        $ readChainPoint
            "74b9b4c63f1af41cd51d74d950cc323a9c159eb76fe52cbd8272dde041c4bdbe@40"
        `shouldNotBe` Nothing

    it "readChainPoint can read \"origin\""
        $ readChainPoint "origin"
        `shouldBe` Just originPoint

    it "Point aeson instances roundtrip" $ do
        let str =
                "74b9b4c63f1af41cd51d74d950cc323a9c159eb76fe52cbd8272dde041c4bdbe@40"
        let p =
                fromMaybe (error "failed reading point") $ readChainPoint str
        Aeson.decode (Aeson.encode p) `shouldBe` Just p

    it "showChainPoint roundtrips through readChainPoint" $
        readChainPoint (showChainPoint originPoint) `shouldBe` Just originPoint

    prop "pickOne picks an element of the list" $ \(Positive entropy)
                                                   (ChainPointSamples samples) ->
        let g = mkStdGen entropy
        in  NE.toList samples `shouldContain` [pickOne g samples]

    prop "different seeds yield different picks" $ \ent ent'
                                                    (ChainPointSamples samples) ->
        let p = pickOne (mkStdGen ent) samples
            p' = pickOne (mkStdGen ent') samples
        in  checkCoverage $
                cover 0.5 (p /= p') "different picks" (property True)

instance Arbitrary ChainPointSamples where
    arbitrary =
        ChainPointSamples . (originPoint NE.:|) <$> listOf (elements samples)
      where
        samples :: [Point]
        samples =
            map
                (fromJust . readChainPoint)
                [ "da6825fc55326d3ca46846b78575d01b711596f280cb111aee9038e7185bd9f0@31"
                , "d59a1df2b509316a85029ed8493d18aac497624d6a8e4f4e9044d21a3f00c10a@95"
                , "65ddb4e6d384fdf97afee9bee47785192cf0aa040764cb4f794d2a57255eb626@100"
                , "6f75bf2de93bba19c7c5092e7a904a9a539461fc5ff99e1556d6ab178e048044@160"
                , "8b5479c8fde9927dff66ba22242b76f821b8c6020fbf673e1afd94118c12fa59@172"
                , "5a47462e396cd20a2c5d15f5eed40b613a38905e6049eaa0b9d3535ded58f44f@224"
                ]
