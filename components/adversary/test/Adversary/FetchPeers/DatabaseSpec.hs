{-# LANGUAGE OverloadedStrings #-}

module Adversary.FetchPeers.DatabaseSpec where

import Adversary.FetchPeers.Database
  ( DatabaseVersionMismatch (..),
    PeerRecord (..),
    closeDatabase,
    loadPeers,
    openDatabase,
    savePeers,
  )
import Control.Exception (bracket)
import Data.Time (getCurrentTime)
import Database.SQLite.Simple (Only (..), execute, execute_, open, query_)
import Network.Socket (SockAddr (..))
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, shouldThrow)

spec :: Spec
spec = do
  describe "Database migrations" $ do
    it "creates migration table on first open" $ do
      withTempDatabase $ \dbPath -> do
        -- Open database (should create migrations table)
        bracket (openDatabase dbPath) closeDatabase $ \_ -> pure ()

        -- Verify migrations table exists and has version 1
        conn <- open dbPath
        version <- query_ conn "SELECT MAX(version) FROM migrations" :: IO [Only (Maybe Int)]
        version `shouldBe` [Only (Just 1)]

    it "applies migration 1 on new database" $ do
      withTempDatabase $ \dbPath -> do
        bracket (openDatabase dbPath) closeDatabase $ \_ -> pure ()

        -- Verify peers table was created
        conn <- open dbPath
        tables <- query_ conn "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name" :: IO [Only String]
        map (\(Only name) -> name) tables `shouldSatisfy` elem "peers"

    it "does not reapply migrations on existing database" $ do
      withTempDatabase $ \dbPath -> do
        -- Open database first time
        bracket (openDatabase dbPath) closeDatabase $ \_ -> pure ()

        -- Get migration count
        conn1 <- open dbPath
        count1 <- query_ conn1 "SELECT COUNT(*) FROM migrations" :: IO [Only Int]

        -- Open database second time
        bracket (openDatabase dbPath) closeDatabase $ \_ -> pure ()

        -- Verify migration count hasn't changed
        conn2 <- open dbPath
        count2 <- query_ conn2 "SELECT COUNT(*) FROM migrations" :: IO [Only Int]
        count2 `shouldBe` count1

    it "can save and load peers after migration" $ do
      withTempDatabase $ \dbPath -> do
        bracket (openDatabase dbPath) closeDatabase $ \db -> do
          -- Save some test peers
          let testPeers =
                [ SockAddrInet 3001 0x0100007f, -- 127.0.0.1:3001
                  SockAddrInet 3002 0x0100007f  -- 127.0.0.1:3002
                ]
          savePeers db testPeers

          -- Load peers back
          loaded <- loadPeers db
          length loaded `shouldBe` 2
          map address loaded `shouldSatisfy` all (`elem` testPeers)

    it "throws DatabaseVersionMismatch when database version is newer than code" $ do
      withTempDatabase $ \dbPath -> do
        -- Create database with a future migration version
        conn <- open dbPath
        execute_ conn "CREATE TABLE migrations (version INTEGER PRIMARY KEY, timestamp TEXT NOT NULL, title TEXT NOT NULL, sql_script TEXT NOT NULL)"
        now <- getCurrentTime
        execute conn "INSERT INTO migrations (version, timestamp, title, sql_script) VALUES (?, ?, ?, ?)" (999 :: Int, now, "Future migration" :: String, "-- future" :: String)

        -- Attempt to open the database should throw an exception
        openDatabase dbPath `shouldThrow` \(DatabaseVersionMismatch databaseVersion codeVersion) -> databaseVersion == 999 && codeVersion < 999

-- | Helper to run tests with a temporary database file
withTempDatabase :: (FilePath -> IO a) -> IO a
withTempDatabase action = do
  tempFile <- emptySystemTempFile "test-peers.db"
  result <- action tempFile
  removeFile tempFile
  pure result
