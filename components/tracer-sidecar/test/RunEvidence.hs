{-# LANGUAGE OverloadedStrings #-}

-- | Loads the log lines recorded by Antithesis run
-- @84894c33e560cf8494b0b78ac8fbaba9-62-14@ (cardano_amaru, one hour,
-- faults enabled) as 'LogMessage's, so the scored properties can be
-- replayed against what the run really produced.
--
-- * @chain-adoptions-run-84894c33.tsv@: every chain-adoption line the
--   sidecar printed (@host added hash\@slot to current chain@), one
--   per row: @at host blockNo slot hash@. Block numbers are
--   reconstructed from the sequence of adoptions per host (the printed
--   lines carry none) and agree with the one block number the run
--   reports (slot 3454 is block 737).
-- * @amaru-relay-lines-run-84894c33.tsv@: the @tip.adopt@ and
--   @ERROR@ lines of @amaru-relay-1@ and @amaru-relay-2@, verbatim,
--   one per row: @host line@.
module RunEvidence
    ( chainAdoptions
    , amaruRelayLines
    , timeline
    )
where

import Cardano.Antithesis.LogMessage
    ( LogMessage (..)
    , mkAmaruLogMessage
    )
import Data.Aeson
    ( Result (..)
    , Value
    , fromJSON
    , object
    , (.=)
    )
import Data.List
    ( sortOn
    )
import Data.Text
    ( Text
    )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
    ( UTCTime
    , defaultTimeLocale
    , parseTimeM
    )

chainAdoptions :: IO [LogMessage]
chainAdoptions =
    map chainAdoption . rows <$> T.readFile chainFile
  where
    chainFile = "test/data/chain-adoptions-run-84894c33.tsv"
    chainAdoption [at, host, blockNo, slot, hash] =
        decodeLogMessage
            $ object
                [ "at" .= at
                , "ns"
                    .= ( "ChainDB.AddBlockEvent.AddedToCurrentChain"
                            :: Text
                       )
                , "sev" .= ("Notice" :: Text)
                , "thread" .= ("25" :: Text)
                , "host" .= host
                , "data"
                    .= object
                        [ "kind" .= ("AddedToCurrentChain" :: Text)
                        , "newtip" .= (hash <> "@" <> slot)
                        , "newSuffixSelectView"
                            .= object
                                [ "kind"
                                    .= ("PraosTiebreakerView" :: Text)
                                , "blockNo" .= (read (T.unpack blockNo) :: Int)
                                , "slotNo" .= (read (T.unpack slot) :: Int)
                                , "issueNo" .= (0 :: Int)
                                , "issuerHash" .= ("" :: Text)
                                , "tieBreakVRF" .= ("" :: Text)
                                , "weightBoost" .= (0 :: Int)
                                ]
                        ]
                ]
    chainAdoption row = error $ "malformed chain row: " <> show row

amaruRelayLines :: IO [LogMessage]
amaruRelayLines =
    map relayLine . rows
        <$> T.readFile "test/data/amaru-relay-lines-run-84894c33.tsv"
  where
    relayLine [relay, line] =
        mkAmaruLogMessage (lineTime line) (relay <> ".log") line
    relayLine row = error $ "malformed relay row: " <> show row

-- | Both streams as the sidecar would have seen them, in time order.
timeline :: [LogMessage] -> [LogMessage]
timeline = sortOn (\LogMessage{at = t} -> t)

rows :: Text -> [[Text]]
rows = map (T.splitOn "\t") . T.lines

lineTime :: Text -> UTCTime
lineTime line =
    case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" stamp of
        Just t -> t
        Nothing -> error $ "no timestamp in: " <> T.unpack line
  where
    stamp = T.unpack (T.takeWhile (/= ' ') line)

decodeLogMessage :: Value -> LogMessage
decodeLogMessage v = case fromJSON v of
    Success m -> m
    Error e -> error e
