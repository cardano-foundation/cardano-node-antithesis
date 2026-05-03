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
import Adversary.SDK qualified as SDK
import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), object, (.=))
import Data.Text qualified as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Point (WithOrigin (..))
import Ouroboros.Network.Point qualified as Point
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
    -- Antithesis composer treats any non-zero exit from a
    -- parallel_driver_*.sh as a finding ("Always: Commands finish
    -- with zero exit code"). All non-zero paths from this point on
    -- are runtime/transient (chaos-killed mid-readFile, malformed
    -- chainpoints file mid-write, signal kills) — none of those
    -- are bugs in the adversary or the system under test. Catch
    -- everything and exit 0; record the cause as a Sometimes(false)
    -- so the report still tracks the rate.
    res <- try @SomeException (loadAndRun args)
    case res of
        Right () -> pure ()
        Left e -> do
            hPutStrLn stderr $
                "cardano-adversary: transient failure: " <> show e
            SDK.sometimes
                False
                "adversary_chain_sync_completed"
                ( object
                    [ "reason" .= T.pack (show e)
                    , "phase" .= T.pack "main"
                    ]
                )
  where
    opts =
        info
            (helper <*> argsParser)
            ( fullDesc
                <> progDesc
                    "Run one initiator-only chain-sync session\
                    \ against one randomly-chosen target node."
            )

loadAndRun :: Args -> IO ()
loadAndRun args = do
    raw <- readFile (argChainPointsFile args)
    case parseChainPointSamples raw of
        Nothing -> do
            hPutStrLn stderr $
                "cardano-adversary: malformed chain-points file: "
                    <> argChainPointsFile args
            -- Treat as transient: tracer-sidecar may be mid-write.
            -- Exit success; the next tick gets a fresh read.
            SDK.sometimes
                False
                "adversary_chain_sync_completed"
                ( object
                    [ "reason" .= T.pack "malformed-or-empty-chainpoints"
                    , "phase" .= T.pack "loadAndRun"
                    ]
                )
        Just (ChainPointSamples points) -> runOnce args points

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
        startDetails =
            object
                [ "target_host" .= T.pack host
                , "point" .= T.pack (showChainPoint point)
                , "limit" .= argLimit args
                ]
    hPutStrLn stderr $
        "cardano-adversary: target=" <> host
            <> " point=" <> showChainPoint point
            <> " limit=" <> show (argLimit args)
            <> " seed=" <> show (argSeed args)
    -- Layer 1 (perturbation #123): prove the binary entered the attack
    -- path. Reachable hits per (start, target_host) tuple — segmented
    -- in the Antithesis report so a host that never gets attacked is
    -- visible as a missing Reachable hit.
    SDK.reachable "adversary_chain_sync_started" startDetails
    -- Per-host coverage fact: the adversary attacked THIS specific
    -- host at least once during the run. Each named ID becomes a
    -- top-level row in the report's Sometimes-assertions group, so
    -- "did every cluster node get attacked at least once?" reads
    -- directly off the table — 5 hits = full coverage, anything
    -- missing means the seed-driven random pick never selected it.
    SDK.sometimes True (perHostId host) startDetails
    t0 <- getCurrentTime
    res <-
        adversaryApplication
            (argMagic args)
            host
            (argPort args)
            point
            (Limit (argLimit args))
    t1 <- getCurrentTime
    let elapsedMs :: Int
        elapsedMs = round (1000 * realToFrac (diffUTCTime t1 t0) :: Double)
        startSlot = slotOf point
    case res of
        Left e -> do
            -- Fault injection (chaos kill, network partition, peer
            -- restart) routinely terminates the session mid-stream.
            -- That's not a bug; log and exit 0 so the test composer
            -- doesn't count the tick as a misconfiguration. Record it
            -- as Sometimes-false for distribution stats — if
            -- Sometimes-true never hits, the adversary is being
            -- killed every time, which is itself signal.
            hPutStrLn stderr $
                "cardano-adversary: session ended early: " <> show e
            -- Stress-coverage line. Logs Explorer (source=adversary.example)
            -- indexes container stdout/stderr; grepping these lines after
            -- a run gives per-host attack rate, per-attack timing, and
            -- a slot-delta proxy for headers actually transferred.
            hPutStrLn stderr $
                "cardano-adversary: stats"
                    <> " target=" <> host
                    <> " start_slot=" <> show startSlot
                    <> " end_slot=-1"
                    <> " slot_delta=-1"
                    <> " elapsed_ms=" <> show elapsedMs
                    <> " outcome=early"
            SDK.sometimes
                False
                "adversary_chain_sync_completed"
                ( object
                    [ "target_host" .= T.pack host
                    , "reason" .= T.pack (show e)
                    , "elapsed_ms" .= elapsedMs
                    , "start_slot" .= startSlot
                    ]
                )
        Right tip -> do
            hPutStrLn stderr $
                "cardano-adversary: completed; reached " <> show tip
            let endSlot = slotOf tip
                slotDelta :: Integer
                slotDelta =
                    fromIntegral endSlot - fromIntegral startSlot
            hPutStrLn stderr $
                "cardano-adversary: stats"
                    <> " target=" <> host
                    <> " start_slot=" <> show startSlot
                    <> " end_slot=" <> show endSlot
                    <> " slot_delta=" <> show slotDelta
                    <> " elapsed_ms=" <> show elapsedMs
                    <> " outcome=ok"
            SDK.sometimes
                True
                "adversary_chain_sync_completed"
                ( object
                    [ "target_host" .= T.pack host
                    , "tip" .= T.pack (show tip)
                    , "elapsed_ms" .= elapsedMs
                    , "start_slot" .= startSlot
                    , "end_slot" .= endSlot
                    , "slot_delta" .= slotDelta
                    ]
                )
            -- Bucket assertions: each fires when the attack lands in
            -- the named regime. Reading the report you can answer
            -- "did the adversary do real work" by checking these
            -- rows are passed: full-sync says some attacks pulled
            -- ≥50 headers; long-attack says some took ≥500ms (i.e.
            -- the network actually had to serve 100 headers, not
            -- just handshake-and-fail).
            let bucketDetails =
                    object
                        [ "target_host" .= T.pack host
                        , "elapsed_ms" .= elapsedMs
                        , "slot_delta" .= slotDelta
                        ]
            if slotDelta >= 50
                then SDK.sometimes
                    True
                    "adversary_full_sync_completed"
                    bucketDetails
                else pure ()
            if elapsedMs < 50
                then SDK.sometimes
                    True
                    "adversary_short_attack_observed"
                    bucketDetails
                else pure ()
            if elapsedMs >= 500
                then SDK.sometimes
                    True
                    "adversary_long_attack_observed"
                    bucketDetails
                else pure ()

-- | Slot number of a chain point, with Origin treated as 0. Used for
-- stress-coverage stats lines so post-run aggregation can compute
-- per-host slot-delta totals (a header-count proxy when blocks land
-- ~1 slot apart).
slotOf :: Point -> Word64
slotOf (Network.Point Origin) = 0
slotOf (Network.Point (At (Point.Block (SlotNo s) _))) = s

-- | Stable per-host Sometimes assertion ID: the part of the FQDN
-- before the first dot. e.g. "p1.example" → "adversary_target_p1_attacked".
-- These IDs are intentionally distinct per host so each becomes its
-- own row in the report — that's how Antithesis surfaces "did every
-- node get attacked" as a checklist.
perHostId :: String -> T.Text
perHostId host = T.pack ("adversary_target_" <> shortName <> "_attacked")
  where
    shortName = takeWhile (/= '.') host
