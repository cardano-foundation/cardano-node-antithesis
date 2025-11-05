module AdversarySpec where

import Adversary
import Adversary.ChainSync (Limit (..))
import Data.Aeson (decode, encode)
import Data.Aeson qualified as Aeson
import Data.Maybe (fromMaybe)
import Test.Hspec (Spec, it, shouldBe, shouldNotBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, Property, forAll)

spec :: Spec
spec = do
    it "Display Message as String"
        $ toString (Startup{arguments = ["Foo"]})
        `shouldBe` "{\"arguments\":[\"Foo\"],\"tag\":\"Startup\"}"

    it "Reads Limit"
        $ read "20"
        `shouldBe` Limit 20

    prop "Roundtrip messages to/from JSON" prop_roundTrip

    it "readChainPoint can read point"
        $ readChainPoint
            "74b9b4c63f1af41cd51d74d950cc323a9c159eb76fe52cbd8272dde041c4bdbe@40"
        `shouldNotBe` Nothing
    it "readChainPoint can read \"origin\""
        $ readChainPoint
            "origin"
        `shouldBe` Just originPoint

    it "Point aeson instances roundtrips" $ do
        let str =
                "74b9b4c63f1af41cd51d74d950cc323a9c159eb76fe52cbd8272dde041c4bdbe@40"
        let p = fromMaybe (error "failed reading point") $ readChainPoint str
        Aeson.decode (Aeson.encode p) `shouldBe` Just p

prop_roundTrip :: Property
prop_roundTrip = forAll genMessage $ \msg ->
    let encoded = encode msg
        decoded = decode encoded
    in  decoded == Just msg

genMessage :: Gen Message
genMessage = Startup <$> arbitrary
