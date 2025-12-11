{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Adversary.FetchPeers.Database
  ( PeerDatabase,
    openDatabase,
    closeDatabase,
    loadPeers,
    savePeers,
    PeerRecord (..),
  )
where

import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple
  ( Connection,
    close,
    execute,
    execute_,
    open,
    query_,
  )
import Network.Socket (SockAddr (..))

-- | Handle to the peer database
newtype PeerDatabase = PeerDatabase Connection

-- | A peer record with address and discovery timestamp
data PeerRecord = PeerRecord
  { prAddress :: SockAddr,
    prTimestamp :: UTCTime
  }
  deriving (Show, Eq)

-- | Open or create a peer database
openDatabase :: FilePath -> IO PeerDatabase
openDatabase path = do
  conn <- open path
  initializeSchema conn
  pure (PeerDatabase conn)

-- | Close the database
closeDatabase :: PeerDatabase -> IO ()
closeDatabase (PeerDatabase conn) = close conn

-- | Initialize the database schema
initializeSchema :: Connection -> IO ()
initializeSchema conn = do
  -- Create peers table with address as TEXT (serialized) and timestamp
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS peers \
    \(address TEXT PRIMARY KEY, \
    \ timestamp TEXT NOT NULL)"

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
