{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- @cardano-adversary@ — one-shot N2N chain-sync misbehaviour CLI.
--
-- Drives a single initiator-only chain-sync session against
-- /one/ target node picked uniformly from @--target-host@, starting
-- from /one/ chain point picked uniformly from
-- @--chain-points-file@. The seed (@--seed@) drives both choices,
-- so the Antithesis hypervisor steers the run via @antithesis_random@
-- in the driver shell.
--
-- Lifecycle: start, attack, exit. No daemon, no socket, no shared
-- state. The composer @docker exec@\'s into a sleeping container that
-- carries this binary; per-tick re-execs are independent.
module Main (main) where

import Adversary
    ( ChainPointSamples (..)
    , Point
    , parseChainPointSamples
    , pickOne
    , showChainPoint
    )
import Adversary.Application
    ( Limit (..)
    , adversaryApplication
    )
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Word (Word32, Word64)
import Network.Socket (PortNumber)
import Numeric (readHex)
import Options.Applicative
    ( Parser
    , auto
    , execParser
    , fullDesc
    , help
    , helper
    , info
    , long
    , metavar
    , option
    , progDesc
    , short
    , some
    , strOption
    , value
    )
import Options.Applicative qualified as Opt
import Ouroboros.Network.Magic (NetworkMagic (..))
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)
import System.Random (mkStdGen, split)

data Args = Args
    { argMagic :: NetworkMagic
    , argHosts :: NonEmpty String
    , argPort :: PortNumber
    , argChainPointsFile :: FilePath
    , argSeed :: Word64
    , argLimit :: Word32
    }

argsParser :: Parser Args
argsParser =
    Args
        <$> ( NetworkMagic
                <$> option
                    auto
                    ( long "network-magic"
                        <> metavar "INT"
                        <> help "Network magic of the target cluster (e.g. 42)."
                    )
            )
        <*> ( NE.fromList
                <$> some
                    ( strOption
                        ( long "target-host"
                            <> metavar "HOST"
                            <> help
                                "Hostname of one candidate target. Pass\
                                \ multiple times; one is picked at random\
                                \ per invocation."
                        )
                    )
            )
        <*> option
            auto
            ( long "target-port"
                <> metavar "PORT"
                <> value 3001
                <> help "N2N port on every --target-host (default: 3001)."
            )
        <*> strOption
            ( long "chain-points-file"
                <> metavar "PATH"
                <> help
                    "Path to a tracer-sidecar chainpoints file.\
                    \ One '<hash>@<slot>' per line (or 'origin')."
            )
        <*> option
            (Opt.eitherReader parseSeed)
            ( long "seed"
                <> metavar "HEX-OR-DEC"
                <> help
                    "PRNG seed driving both target-host and chain-point\
                    \ pick. Accepts decimal or 0x-prefixed hex. Source\
                    \ this from $(antithesis_random) in the driver."
            )
        <*> option
            auto
            ( long "limit"
                <> short 'l'
                <> metavar "N"
                <> value 100
                <> help "Stop after syncing N headers (default: 100)."
            )

parseSeed :: String -> Either String Word64
parseSeed s = case s of
    '0' : 'x' : hex -> case readHex hex of
        [(n, "")] -> Right n
        _ -> Left $ "not a hex uint64: " <> s
    _ -> case reads s of
        [(n :: Word64, "")] -> Right n
        _ -> Left $ "not a uint64: " <> s

main :: IO ()
main = do
    args <- execParser opts
    raw <- readFile (argChainPointsFile args)
    case parseChainPointSamples raw of
        Nothing -> do
            hPutStrLn stderr $
                "cardano-adversary: malformed chain-points file: "
                    <> argChainPointsFile args
            exitWith (ExitFailure 1)
        Just (ChainPointSamples points) -> runOnce args points
  where
    opts =
        info
            (helper <*> argsParser)
            ( fullDesc
                <> progDesc
                    "Run one initiator-only chain-sync session\
                    \ against one randomly-chosen target node."
            )

-- | Pick one host and one starting point from the seed, then run a
-- single 'adversaryApplication' against that pair.
--
-- Two independent generators (split from the seed) ensure the host
-- pick does not bias the point pick or vice-versa.
runOnce :: Args -> NonEmpty Point -> IO ()
runOnce args points = do
    let (gHost, gPoint) = split (mkStdGen (fromIntegral (argSeed args)))
        host = pickOne gHost (argHosts args)
        point = pickOne gPoint points
    hPutStrLn stderr $
        "cardano-adversary: target=" <> host
            <> " point=" <> showChainPoint point
            <> " limit=" <> show (argLimit args)
            <> " seed=" <> show (argSeed args)
    res <-
        adversaryApplication
            (argMagic args)
            host
            (argPort args)
            point
            (Limit (argLimit args))
    case res of
        Left e ->
            -- Fault injection (chaos kill, network partition, peer
            -- restart) routinely terminates the session mid-stream.
            -- That's not a bug; log and exit 0 so the test composer
            -- doesn't count the tick as a misconfiguration.
            hPutStrLn stderr $
                "cardano-adversary: session ended early: " <> show e
        Right tip ->
            hPutStrLn stderr $
                "cardano-adversary: completed; reached " <> show tip
