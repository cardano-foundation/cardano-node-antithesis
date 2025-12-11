module Adversary.Run (run, Command (..), programInfo) where

import Adversary (ChainSyncOptions (..), Message (..), adversary, toString)
import Adversary.FetchPeers (FetchPeersOptions (..), fetchPeers)
import Adversary.FetchPeers.Log (FetchLog)
import Adversary.SubmitTransactions (SubmitOptions (..), submitTxs)
import Adversary.SubmitTransactions.Log (SubmitLog)
import Control.Tracer (contramap, stdoutTracer)
import Data.List.NonEmpty qualified as NE
import Options.Applicative
import System.Environment (getArgs)

-- | Command-line options for the adversary program
data Command
  = ChainSyncCommand ChainSyncOptions
  | SubmitCommand SubmitOptions
  | FetchPeersCommand FetchPeersOptions
  deriving (Show, Eq)

-- | Parser for ChainSync options
chainSyncOptionsParser :: Parser ChainSyncOptions
chainSyncOptionsParser =
  ChainSyncOptions
    <$> option
      auto
      ( long "network-magic"
          <> short 'm'
          <> metavar "MAGIC"
          <> help "Network magic number (e.g., 42 for testnet, 764824073 for mainnet)"
      )
    <*> option
      auto
      ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> help "Port number to connect to"
      )
    <*> option
      auto
      ( long "sync-length"
          <> short 'l'
          <> metavar "LENGTH"
          <> help "Maximum number of headers to sync"
      )
    <*> strOption
      ( long "chain-points-file"
          <> short 'f'
          <> metavar "FILE"
          <> help "File containing chain points (one per line: hash@slot or 'origin')"
      )
    <*> option
      auto
      ( long "num-connections"
          <> short 'n'
          <> metavar "NUM"
          <> help "Number of simultaneous connections to open"
      )
    <*> ( NE.fromList
            <$> some
              ( strArgument
                  ( metavar "HOSTS..."
                      <> help "List of hosts to connect to"
                  )
              )
        )

-- | Parser for Submit options
submitOptionsParser :: Parser SubmitOptions
submitOptionsParser =
  SubmitOptions
    <$> option
      auto
      ( long "network-magic"
          <> short 'm'
          <> metavar "MAGIC"
          <> help "Network magic number (e.g., 42 for testnet, 764824073 for mainnet)"
      )
    <*> strOption
      ( long "host"
          <> short 'h'
          <> metavar "HOST"
          <> help "Host to connect to"
      )
    <*> option
      auto
      ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> help "Port number to connect to"
      )
    <*> ( NE.fromList
            <$> some
              ( strArgument
                  ( metavar "TX-FILES..."
                      <> help "Transaction files to monitor (hex-encoded CBOR)"
                  )
              )
        )

-- | Parser for FetchPeers options
fetchPeersOptionsParser :: Parser FetchPeersOptions
fetchPeersOptionsParser =
  FetchPeersOptions
    <$> option
      auto
      ( long "network-magic"
          <> short 'm'
          <> metavar "MAGIC"
          <> help "Network magic number (e.g., 42 for testnet, 764824073 for mainnet)"
      )
    <*> strOption
      ( long "initial-peer"
          <> short 'i'
          <> metavar "HOST:PORT"
          <> help "Initial peer to connect to (format: host:port)"
      )
    <*> option
      auto
      ( long "poll-interval"
          <> short 't'
          <> metavar "SECONDS"
          <> help "Polling interval in seconds"
          <> value 60
          <> showDefault
      )
    <*> option
      auto
      ( long "num-peers"
          <> short 'n'
          <> metavar "NUM"
          <> help "Number of peers to request per poll"
          <> value 10
          <> showDefault
      )
    <*> strOption
      ( long "peers-database"
          <> short 'd'
          <> metavar "PATH"
          <> help "Path to SQLite database file for peer persistence"
          <> value "peers.db"
          <> showDefault
      )

-- | Command parser with sub-commands
commandParser :: Parser Command
commandParser =
  hsubparser
    ( command
        "chainsync"
        ( info
            (ChainSyncCommand <$> chainSyncOptionsParser)
            (progDesc "Connect to Cardano nodes and sync blocks using ChainSync protocol")
        )
        <> command
          "submit"
          ( info
              (SubmitCommand <$> submitOptionsParser)
              (progDesc "Submit transactions by polling transaction files")
          )
        <> command
          "fetch-peers"
          ( info
              (FetchPeersCommand <$> fetchPeersOptionsParser)
              (progDesc "Fetch and maintain a list of peers using the PeerSharing protocol")
          )
    )

-- | Program info with header and description
programInfo :: ParserInfo Command
programInfo =
  info
    (commandParser <**> helper)
    ( fullDesc
        <> progDesc "Adversary tool for Cardano Antithesis testing"
        <> header "adversary - Cardano chaos testing tool implementing N2N protocol"
    )

-- | Main entry point that parses arguments and runs the appropriate command
run :: IO Message
run = do
  args <- getArgs
  putStrLn $ toString $ Startup args
  cmd <- execParser programInfo
  case cmd of
    ChainSyncCommand opts ->
      adversary opts
    SubmitCommand opts ->
      submitTxs (contramap (show :: SubmitLog -> String) stdoutTracer) opts
    FetchPeersCommand opts ->
      fetchPeers (contramap (show :: FetchLog -> String) stdoutTracer) opts
