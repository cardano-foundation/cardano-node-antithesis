module Adversary.Run (run, Command (..), ChainSyncOptions (..), SubmitOptions (..), programInfo) where

import Adversary (Message (..), adversary)
import Adversary.SubmitTransactions (submitTxs)
import Control.Tracer (contramap, stdoutTracer)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Word (Word64)
import Options.Applicative

-- | Command-line options for the adversary program
data Command
  = ChainSyncCommand ChainSyncOptions
  | SubmitCommand SubmitOptions
  deriving (Show, Eq)

-- | Options for the ChainSync sub-command
data ChainSyncOptions = ChainSyncOptions
  { csNetworkMagic :: Word64
  , csPort :: Int
  , csSyncLength :: Int
  , csChainPointsFile :: FilePath
  , csNumConnections :: Int
  , csHosts :: NonEmpty String
  }
  deriving (Show, Eq)

-- | Options for the Submit sub-command
data SubmitOptions = SubmitOptions
  { submitNetworkMagic :: Word64
  , submitHost :: String
  , submitPort :: Int
  , submitTxFiles :: NonEmpty FilePath
  }
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

-- | Convert options to string arguments for backward compatibility
chainSyncOptionsToArgs :: ChainSyncOptions -> [String]
chainSyncOptionsToArgs ChainSyncOptions {..} =
  [ show csNetworkMagic
  , show csPort
  , show csSyncLength
  , csChainPointsFile
  , show csNumConnections
  ]
    ++ NE.toList csHosts

-- | Convert options to string arguments for backward compatibility
submitOptionsToArgs :: SubmitOptions -> [String]
submitOptionsToArgs SubmitOptions {..} =
  [ show submitNetworkMagic
  , submitHost
  , show submitPort
  ]
    ++ NE.toList submitTxFiles

-- | Main entry point that parses arguments and runs the appropriate command
run :: IO Message
run = do
  cmd <- execParser programInfo
  case cmd of
    ChainSyncCommand opts ->
      adversary (chainSyncOptionsToArgs opts)
    SubmitCommand opts ->
      submitTxs (contramap show stdoutTracer) (submitOptionsToArgs opts)
