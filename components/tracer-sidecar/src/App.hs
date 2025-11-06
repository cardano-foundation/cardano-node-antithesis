{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App
    ( main
    , tailJsonLinesFromTracerLogDir
    )
where

import Cardano.Antithesis.Sdk
import Cardano.Antithesis.Sidecar

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL

import Control.Concurrent
    ( modifyMVar_
    , newMVar
    , threadDelay
    )
import Control.Concurrent.Async (async, link, wait)
import Control.Monad
    ( filterM
    , forM
    , forM_
    , forever
    , (<=<)
    )
import Data.Aeson
    ( FromJSON
    , eitherDecode
    )
import Data.Foldable (traverse_)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Directory
    ( listDirectory
    , pathIsSymbolicLink
    )
import System.Environment
    ( getArgs
    , getEnv
    )
import System.FilePath
    ( (</>)
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
        Left _e -> pure () -- putStrLn $ "warning: unrecognized line: " <> B8.unpack bs <> " " <> show e
    go
        :: Set FilePath
        -> IO ()
    go seen = do
        let sample = do
                newSet <- nodeLogFiles
                let newFiles = newSet `Set.difference` seen
                if null newFiles
                    then do
                        threadDelay 10000
                        sample
                    else return newFiles
        newFiles <- sample
        forM_ newFiles $ \path -> link <=< async $ do
            withFile path ReadMode $ \h -> do
                forever $ do
                    eof <- hIsEOF h
                    if eof
                        then do
                            threadDelay 10000
                        else do
                            l <- B8.hGetLine h
                            callback l
        go (seen <> newFiles)
      where
        -- switch to unbuffered mode and follow new data

        nodeLogFiles :: IO (Set FilePath)
        nodeLogFiles = do
            entries <- fmap (dir </>) <$> listDirectory dir
            logFiles <- filterM (fmap not . pathIsSymbolicLink) entries
            pure $ Set.fromList logFiles
