{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Spec (spec)
where

import Cardano.Antithesis.Sidecar
    ( Output (..)
    , initialState
    , mkSpec
    , processMessages
    )
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL

import App (tailJsonLinesFromTracerLogDir)
import Cardano.Antithesis.LogMessage (LogMessage)
import Control.Concurrent
    ( MVar
    , modifyMVar_
    , newMVar
    , readMVar
    , threadDelay
    )
import Control.Concurrent.Async (async, cancel)
import Data.Aeson
    ( ToJSON (toJSON)
    , Value
    , decodeStrict'
    , eitherDecodeStrict
    , encode
    )
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Either
    ( partitionEithers
    )
import Data.Foldable (forM_)
import Data.Maybe
    ( mapMaybe
    )
import System.FilePath ((</>))
import System.IO.Temp
    ( withSystemTempDirectory
    )
import System.Process (system)
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

    describe "tailJsonLinesFromTracerLogDir" $ do
        it "works on 10 files with 10 values"
            $ withSystemTempDirectory "tracer-log-dir"
            $ \dir -> do
                r <- newMVar []
                thread <-
                    async
                        $ tailJsonLinesFromTracerLogDir dir
                        $ collectAllInts r
                let nFiles = 10
                let nValues = 10
                simulateRestartingNodeTracer nFiles nValues dir
                threadDelay 500000
                cancel thread
                vals <- readMVar r
                vals `shouldBe` concat (replicate nFiles [1 .. nValues])

collectAllInts :: MVar [Int] -> Int -> IO ()
collectAllInts xs newInt =
    modifyMVar_ xs $ \ints -> pure (ints <> [newInt])

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

generateAFileWithJSONLines :: FilePath -> [Int] -> IO ()
generateAFileWithJSONLines fp xs = do
    forM_ xs $ \x -> do
        let value = BL8.unpack $ encode x
        _ <- system $ "echo " <> value <> " >> " <> fp
        threadDelay 1000

-- less than 10000 files may be generated
generateFilenamesInLexicalOrder :: FilePath -> Int -> [FilePath]
generateFilenamesInLexicalOrder dir n =
    [dir </> "file-" <> padZeroes i <> ".json" | i <- [1 .. n]]
  where
    padZeroes i = let s = show i in replicate (4 - length s) '0' <> s

generateStreamOfFiles :: [FilePath] -> (FilePath -> IO ()) -> IO ()
generateStreamOfFiles files action = do
    forM_ files $ \file -> do
        action file
        threadDelay 10000

simulateRestartingNodeTracer
    :: Int -- number of files
    -> Int -- number of values per file
    -> FilePath -- directory to write files into
    -> IO ()
simulateRestartingNodeTracer nFiles nValues dir = do
    let files = generateFilenamesInLexicalOrder dir nFiles
    generateStreamOfFiles files $ \fp -> do
        generateAFileWithJSONLines fp [1 .. nValues]
