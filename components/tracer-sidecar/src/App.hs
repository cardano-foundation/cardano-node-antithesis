{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use when" #-}

module App
    ( main
    , tailJsonLinesFromTracerLogDir
    )
where

import Cardano.Antithesis.Sdk
    ( sometimesFailed
    , sometimesTracesDeclaration
    , writeSdkJsonl
    )
import Cardano.Antithesis.Sidecar
    ( initialStateIO
    , mkSpec
    , processMessageIO
    )
import Control.Concurrent
    ( modifyMVar_
    , newMVar
    , threadDelay
    )
import Control.Concurrent.Async (async, link, wait)
import Control.Exception (SomeException, try)
import Control.Monad
    ( forM
    , forM_
    , forever
    , guard
    , when
    , (<=<)
    )
import Control.Monad.Fix (fix)
import Data.Aeson
    ( FromJSON
    , ToJSON (..)
    , eitherDecode
    )
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import System.Directory
    ( listDirectory
    )
import System.Environment
    ( getArgs
    , getEnv
    )
import System.FilePath
    ( takeFileName
    , (</>)
    )
import System.IO
    ( BufferMode (LineBuffering)
    , IOMode (..)
    , SeekMode (SeekFromEnd)
    , hIsEOF
    , hSeek
    , hSetBuffering
    , stdout
    , withFile
    )

-- main ------------------------------------------------------------------------

-- | Main: <program> <directory>
--  Processes existing .jsonl files and tails them for new entries.
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    putStrLn "starting tracer-sidecar..."
    args <- getArgs
    dir <- case args of
        [d] -> return d
        _ -> error "Usage: <executable name> <directory>"

    (nPools :: Int) <- read <$> getEnv "POOLS"

    writeSdkJsonl $ sometimesTracesDeclaration "find log files"

    nodeDirs <- fix $ \loop -> do
        mls <- try $ listDirectory dir
        case mls of
            Left (e :: SomeException) -> do
                putStrLn $ "Error listing directory " <> dir <> ": " <> show e
                threadDelay 1000000
                loop
            Right _ -> do
                nodeDirs' <- fmap (dir </>) <$> listDirectory dir
                if null nodeDirs'
                    then do
                        putStrLn "No node log directories found, waiting..."
                        threadDelay 1000000
                        loop
                    else return nodeDirs'
    let spec = mkSpec nPools

    writeSdkJsonl $ sometimesFailed "find log files" $ toJSON nodeDirs

    mvar <- newMVar =<< initialStateIO spec
    threads <- forM nodeDirs $ \nodeDir ->
        async
            $ tailJsonLinesFromTracerLogDir
                True
                nodeDir
                (modifyMVar_ mvar . flip (processMessageIO spec))
    traverse_ wait threads

-- | Tails json lines from each file in a directory,
-- one at a time respecting their lexical order
-- and waiting for new files to appear forever.
-- We rely on the invariant that the filename order reflects the order of arrival
-- in the directory
tailJsonLinesFromTracerLogDir
    :: FromJSON a
    => Bool
    -> FilePath -- directory to watch
    -> (a -> IO ()) -- action on each decoded log message
    -> IO ()
tailJsonLinesFromTracerLogDir skip dir action = go mempty
  where
    callback bs = case eitherDecode $ BL.fromStrict bs of
        Right msg -> action msg
        Left _e -> pure () -- ignore decode errors
    go
        :: Set FilePath
        -> IO ()
    go seen = do
        let sample = do
                newSet <- nodeLogFiles
                let newFiles = newSet `Set.difference` seen
                if null newFiles
                    then do
                        threadDelay 1000000
                        sample
                    else return newFiles
        newFiles <- sample
        forM_ newFiles $ \path -> link <=< async $ do
            putStrLn $ "Tailing file: " <> path
            exited <- try
                $ withFile path ReadMode
                $ \h -> do
                    hSetBuffering h LineBuffering
                    when skip
                        $ hSeek h SeekFromEnd 0
                    forever $ do
                        eof <- hIsEOF h
                        if eof
                            then do
                                threadDelay 10000
                            else do
                                l <- B8.hGetLine h
                                callback l
            case exited of
                Left (e :: SomeException) ->
                    putStrLn
                        $ "Error reading file "
                            <> path
                            <> ": "
                            <> show e
                Right _ -> print $ "Finished reading file: " <> path
        go (seen <> newFiles)
      where
        -- switch to unbuffered mode and follow new data

        nodeLogFiles :: IO (Set FilePath)
        nodeLogFiles = do
            entries <- fmap (dir </>) <$> listDirectory dir
            let logFiles = filter parsingNodeLogFile entries
            pure $ Set.fromList logFiles

parsingNodeLogFile :: FilePath -> Bool
parsingNodeLogFile path = isJust $ do
    let filename = takeFileName path
    guard $ isPrefixOf "node-" filename
    let timestamp = drop 5 $ takeWhile (/= '.') filename
    parseTimeM @_ @UTCTime
        True
        defaultTimeLocale
        "%Y-%m-%dT%H-%M-%S"
        timestamp
