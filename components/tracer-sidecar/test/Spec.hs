{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Spec (spec)
where

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL

import Cardano.Antithesis.LogMessage (LogMessage)
import Cardano.Antithesis.Sidecar
    ( Output (..)
    , initialState
    , mkSpec
    , processMessages
    )

import Data.Aeson
    ( ToJSON (toJSON)
    , Value
    , decodeStrict'
    , eitherDecodeStrict
    , encode
    )
import Data.Either
    ( partitionEithers
    )
import Data.Maybe
    ( mapMaybe
    )
import Test.Hspec
import Test.Hspec.Golden
    ( Golden (..)
    )

spec :: Spec
spec = do
    (input :: [B8.ByteString]) <-
        runIO $ B8.lines <$> B8.readFile "test/data/input.jsonl"

    it "processMessages"
        $ let
            propSpec = mkSpec 3
            (_finalState, actualVals) = processMessages propSpec (initialState propSpec) msgs
            msgs = mapMaybe decodeStrict' input
          in
            myGoldenTest (map jsonifyOutput actualVals)

    it "all test data messages can be decoded" $ do
        let (errs, _res) = partitionEithers $ map (eitherDecodeStrict @LogMessage) input
        case errs of
            ["Unexpected end-of-input, expecting record key literal or }"] -> pure ()
            [] -> pure ()
            _ ->
                expectationFailure
                    $ "Some messages couldn't be decoded: " <> show errs

jsonifyOutput :: Output -> Value
jsonifyOutput (StdOut msg) = toJSON $ "### STDOUT: " <> msg
jsonifyOutput (AntithesisSdk v) = v
jsonifyOutput (RecordChainPoint msg) = toJSON $ "### chainPoints.log: " <> msg

myGoldenTest :: [Value] -> Golden [Value]
myGoldenTest actualOutput =
    Golden
        { output = actualOutput
        , encodePretty = B8.unpack . encodeJsonl
        , writeToFile = \fp -> B8.writeFile fp . encodeJsonl
        , readFromFile = fmap (mapMaybe decodeStrict' . B8.lines) . B8.readFile
        , goldenFile = "test/data/output.jsonl"
        , actualFile = Just "test/data/output-actual.jsonl"
        , failFirstTime = False
        }
  where
    encodeJsonl = B8.intercalate "\n" . map (BL.toStrict . encode)
