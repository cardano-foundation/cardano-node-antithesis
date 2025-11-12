{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module App
    ( main
    , tailJsonLinesFromTracerLogDir
    )
where

import Cardano.Antithesis.Sdk
    ( sometimesTracesDeclaration
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
    , (<=<)
    )
import Data.Aeson
    ( FromJSON
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
    , hIsEOF
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

    writeSdkJsonl $ sometimesTracesDeclaration "finds all node log files"

    nodeDirs <- fmap (dir </>) <$> listDirectory dir

    let spec = mkSpec nPools

    mvar <- newMVar =<< initialStateIO spec

    threads <- forM nodeDirs $ \nodeDir ->
        async
            $ tailJsonLinesFromTracerLogDir
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
    => FilePath -- directory to watch
    -> (a -> IO ()) -- action on each decoded log message
    -> IO ()
tailJsonLinesFromTracerLogDir dir action = go mempty
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
