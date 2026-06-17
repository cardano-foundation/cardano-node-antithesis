{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Spec (spec)
where

import App (tailJsonLinesFromTracerLogDir)
import Cardano.Antithesis.LogMessage
    ( LogMessage (..)
    , LogMessageData (..)
    , NewTipSelectView (..)
    , Severity (..)
    )
import Cardano.Antithesis.Sidecar
    ( Output (..)
    , initialState
    , mkSpec
    , processMessages
    )
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
    , Value (..)
    , decodeStrict'
    , eitherDecodeStrict
    , encode
    , object
    , (.=)
    )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Either
    ( partitionEithers
    )
import Data.Foldable (forM_)
import Data.List
    ( sort
    )
import Data.Maybe
    ( mapMaybe
    )
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import ForkTreeSpec qualified
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
    ForkTreeSpec.spec
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

    describe "cluster fork-depth assertion" $ do
        it "does not count stale relay lag as producer fork depth" $ do
            let propSpec = mkSpec 3
                msgs =
                    added "relay1" "root" 0
                        : sameProducerChain 432
                (_finalState, actualVals) =
                    processMessages propSpec (initialState propSpec) msgs

            failedAssertionIds actualVals
                `shouldNotContain` ["cluster fork depth < k"]

        it "still fails when producers diverge by k" $ do
            let propSpec = mkSpec 3
                (_finalState, actualVals) =
                    processMessages propSpec (initialState propSpec)
                        $ divergentProducerChains 432

            failedAssertionIds actualVals
                `shouldContain` ["cluster fork depth < k"]

    describe "tailJsonLinesFromTracerLogDir" $ do
        it "works on 10 files with 10 values"
            $ withSystemTempDirectory "tracer-log-dir"
            $ \dir -> do
                r <- newMVar []
                thread <-
                    async
                        $ tailJsonLinesFromTracerLogDir False dir
                        $ collectAllInts r
                let nFiles = 10
                let nValues = 130
                threadDelay 500000
                simulateRestartingNodeTracer nFiles nValues dir
                threadDelay 500000
                cancel thread
                vals <- readMVar r
                sort vals `shouldBe` sort (concat $ replicate nFiles [1 .. nValues])

collectAllInts :: MVar [Int] -> Int -> IO ()
collectAllInts xs newInt = do
    modifyMVar_ xs $ \ints -> pure (ints <> [newInt])

jsonifyOutput :: Output -> Value
jsonifyOutput (StdOut msg) = toJSON $ "### STDOUT: " <> msg
jsonifyOutput (AntithesisSdk v) = v
jsonifyOutput (RecordChainPoint msg) = toJSON $ "### chainPoints.log: " <> msg

failedAssertionIds :: [Output] -> [Text]
failedAssertionIds outputs =
    [ assertionId
    | AntithesisSdk (Object o) <- outputs
    , Just (Object assertion) <-
        [KeyMap.lookup (Key.fromString "antithesis_assert") o]
    , Just (Bool True) <- [KeyMap.lookup (Key.fromString "hit") assertion]
    , Just (Bool False) <-
        [KeyMap.lookup (Key.fromString "condition") assertion]
    , Just (String assertionId) <-
        [KeyMap.lookup (Key.fromString "id") assertion]
    ]

sameProducerChain :: Int -> [LogMessage]
sameProducerChain depth =
    [added host "root" 0 | host <- producerHosts]
        <> concat
            [ [added host ("h" <> T.pack (show n)) n | host <- producerHosts]
            | n <- [1 .. depth]
            ]

divergentProducerChains :: Int -> [LogMessage]
divergentProducerChains depth =
    [added host "root" 0 | host <- producerHosts]
        <> concat
            [ [ added "p1.example" ("a" <> T.pack (show n)) n
              , added "p2" ("b" <> T.pack (show n)) n
              , added "p3" ("c" <> T.pack (show n)) n
              ]
            | n <- [1 .. depth]
            ]

producerHosts :: [Text]
producerHosts = ["p1.example", "p2", "p3"]

added :: Text -> Text -> Int -> LogMessage
added host hash chainLength =
    LogMessage
        { at = UTCTime (fromGregorian 2026 5 8) 0
        , ns = "ChainDB.AddBlockEvent.AddedToCurrentChain"
        , details =
            AddedToCurrentChain
                { newTipSelectView = tipSelectView chainLength
                , newtip = hash <> "@" <> T.pack (show chainLength)
                }
        , sev = Notice
        , thread = "test"
        , host = host
        , kind = "AddedToCurrentChain"
        , json =
            object
                [ "host" .= host
                , "data"
                    .= object
                        [ "kind" .= ("AddedToCurrentChain" :: Text)
                        , "newtip" .= (hash <> "@" <> T.pack (show chainLength))
                        ]
                ]
        }

tipSelectView :: Int -> NewTipSelectView
tipSelectView chainLength =
    NewTipSelectView
        { chainLength = chainLength
        , issueNo = 0
        , issuerHash = "issuer"
        , kind = "PraosTiebreakerView"
        , slotNo = chainLength
        , tieBreakVRF = "vrf"
        }

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

generateIncreasingUTCTimes
    :: Int -> UTCTime -> NominalDiffTime -> [UTCTime]
generateIncreasingUTCTimes n start delta =
    take n $ iterate (addUTCTime delta) start

renderFilenameWithUTCTime :: UTCTime -> FilePath
renderFilenameWithUTCTime =
    (<> ".json")
        . ("node-" <>)
        . formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S"

-- less than 10000 files may be generated
generateFilenamesInLexicalOrder :: FilePath -> Int -> [FilePath]
generateFilenamesInLexicalOrder dir n =
    [dir </> renderFilenameWithUTCTime t | t <- times]
  where
    times =
        generateIncreasingUTCTimes
            n
            (UTCTime (fromGregorian 2025 11 1) (secondsToDiffTime 0))
            3890

generateStreamOfFiles :: [FilePath] -> (FilePath -> IO ()) -> IO ()
generateStreamOfFiles files action = do
    forM_ files $ \file -> do
        action file
        threadDelay 100000

simulateRestartingNodeTracer
    :: Int -- number of files
    -> Int -- number of values per file
    -> FilePath -- directory to write files into
    -> IO ()
simulateRestartingNodeTracer nFiles nValues dir = do
    let files = generateFilenamesInLexicalOrder dir nFiles
    generateStreamOfFiles files $ \fp -> do
        generateAFileWithJSONLines fp [1 .. nValues]
