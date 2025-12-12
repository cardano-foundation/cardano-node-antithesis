{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Adversary.FetchPeers.Database
  ( PeerDatabase,
    openDatabase,
    closeDatabase,
    loadPeers,
    savePeers,
    PeerRecord (..),
    Migration (..),
    DatabaseVersionMismatch (..),
  )
where

import Control.Exception (Exception, throwIO)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.String (fromString)
import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple
  ( Connection,
    Only (..),
    close,
    execute,
    execute_,
    open,
    query_,
  )
import Database.SQLite.Simple.FromRow (FromRow (..), field)
import Network.Socket (SockAddr (..))
import System.IO (hPutStrLn, stderr)

-- | Handle to the peer database
newtype PeerDatabase = PeerDatabase Connection

-- | A peer record with address and discovery timestamp
data PeerRecord = PeerRecord
  { address :: SockAddr,
    timestamp :: UTCTime
  }
  deriving (Show, Eq)

-- | A database migration
data Migration = Migration
  { version :: Int,
    title :: String,
    sql :: String
  }
  deriving (Show, Eq)

-- | Exception thrown when database version is newer than code version
data DatabaseVersionMismatch = DatabaseVersionMismatch
  { databaseVersion :: Int,
    codeVersion :: Int
  }
  deriving (Show, Eq)

instance Exception DatabaseVersionMismatch

-- | Database migration record from the migrations table
data MigrationRecord = MigrationRecord
  { version :: Int,
    timestamp :: UTCTime,
    title :: String,
    sql :: String
  }
  deriving (Show, Eq)

instance FromRow MigrationRecord where
  fromRow = MigrationRecord <$> field <*> field <*> field <*> field

-- | List of all database migrations in ascending version order
--   New migrations should be appended to the end of this list
migrations :: [Migration]
migrations =
  [ Migration
      { version = 1,
        title = "Create peers table",
        sql =
          "CREATE TABLE IF NOT EXISTS peers \
          \(address TEXT PRIMARY KEY, \
          \ timestamp TEXT NOT NULL)"
      }
  ]

-- | Get the latest migration version from the code
latestMigrationVersion :: Int
latestMigrationVersion = case migrations of
  [] -> 0
  _ -> maximum (map (\Migration {version = v} -> v) migrations)

-- | Open or create a peer database
openDatabase :: FilePath -> IO PeerDatabase
openDatabase path = do
  conn <- open path
  initializeSchema conn
  pure (PeerDatabase conn)

-- | Close the database
closeDatabase :: PeerDatabase -> IO ()
closeDatabase (PeerDatabase conn) = close conn

-- | Initialize the database schema and apply migrations
initializeSchema :: Connection -> IO ()
initializeSchema conn = do
  -- Create the migrations table if it doesn't exist
  createMigrationsTable conn

  -- Get the current database version
  dbVer <- getDatabaseVersion conn

  -- Get the latest code version
  let codeVer = latestMigrationVersion

  -- Compare versions and handle accordingly
  case compare dbVer codeVer of
    EQ -> pure () -- Versions match, nothing to do
    LT -> applyPendingMigrations conn dbVer codeVer -- Database is behind, apply migrations
    GT -> throwIO $ DatabaseVersionMismatch {databaseVersion = dbVer, codeVersion = codeVer} -- Database is ahead of code version

-- | Create the migrations table if it doesn't exist
createMigrationsTable :: Connection -> IO ()
createMigrationsTable conn =
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS migrations \
    \(version INTEGER PRIMARY KEY, \
    \ timestamp TEXT NOT NULL, \
    \ title TEXT NOT NULL, \
    \ sql_script TEXT NOT NULL)"

-- | Get the current database version (highest version in migrations table)
getDatabaseVersion :: Connection -> IO Int
getDatabaseVersion conn = do
  result <- query_ conn "SELECT MAX(version) FROM migrations" :: IO [Only (Maybe Int)]
  case result of
    [Only (Just v)] -> pure v
    _ -> pure 0 -- No migrations applied yet

-- | Apply all pending migrations from dbVersion+1 to codeVersion
applyPendingMigrations :: Connection -> Int -> Int -> IO ()
applyPendingMigrations conn dbVer codeVer = do
  let pending = filter (\Migration {version = v} -> v > dbVer && v <= codeVer) migrations
  let sorted = sortBy (comparing (\Migration {version = v} -> v)) pending
  mapM_ (applyMigration conn) sorted

-- | Apply a single migration
applyMigration :: Connection -> Migration -> IO ()
applyMigration conn Migration {..} = do
  hPutStrLn stderr $ "Applying migration " ++ show version ++ ": " ++ title
  -- Execute the migration SQL
  execute_ conn (fromString sql)
  -- Record the migration in the migrations table
  now <- getCurrentTime
  execute
    conn
    "INSERT INTO migrations (version, timestamp, title, sql_script) VALUES (?, ?, ?, ?)"
    (version, now, title, sql)

-- | Load all peers from the database
loadPeers :: PeerDatabase -> IO [PeerRecord]
loadPeers (PeerDatabase conn) = do
  rows <- query_ conn "SELECT address, timestamp FROM peers" :: IO [(String, UTCTime)]
  pure [PeerRecord (deserializeSockAddr addr) ts | (addr, ts) <- rows]

-- | Save new peers to the database (insert or replace)
savePeers :: PeerDatabase -> [SockAddr] -> IO ()
savePeers (PeerDatabase conn) addrs = do
  now <- getCurrentTime
  mapM_ (savePeer conn now) addrs

savePeer :: Connection -> UTCTime -> SockAddr -> IO ()
savePeer conn timestamp addr = do
  let addrStr = serializeSockAddr addr
  execute
    conn
    "INSERT OR REPLACE INTO peers (address, timestamp) VALUES (?, ?)"
    (addrStr, timestamp)

-- | Serialize a SockAddr to a string for storage
serializeSockAddr :: SockAddr -> String
serializeSockAddr (SockAddrInet port host) =
  "ipv4:" ++ show host ++ ":" ++ show port
serializeSockAddr (SockAddrInet6 port _flow (w1, w2, w3, w4) _scope) =
  "ipv6:"
    ++ show w1
    ++ ":"
    ++ show w2
    ++ ":"
    ++ show w3
    ++ ":"
    ++ show w4
    ++ ":"
    ++ show port
serializeSockAddr (SockAddrUnix path) =
  "unix:" ++ path

-- | Deserialize a SockAddr from a string
deserializeSockAddr :: String -> SockAddr
deserializeSockAddr str = case break (== ':') str of
  ("ipv4", rest) -> case break (== ':') (drop 1 rest) of
    (hostStr, ':' : portStr) ->
      SockAddrInet (read portStr) (read hostStr)
    _ -> error $ "Invalid IPv4 address format: " ++ str
  ("ipv6", rest) ->
    let parts = splitOn ':' (drop 1 rest)
     in case parts of
          [w1, w2, w3, w4, port] ->
            SockAddrInet6
              (read port)
              0
              (read w1, read w2, read w3, read w4)
              0
          _ -> error $ "Invalid IPv6 address format: " ++ str
  ("unix", rest) -> SockAddrUnix (drop 1 rest)
  _ -> error $ "Unknown address type: " ++ str

-- | Split a string on a character
splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c s =
  let (chunk, rest) = break (== c) s
   in chunk : case rest of
        [] -> []
        (_ : rest') -> splitOn c rest'
