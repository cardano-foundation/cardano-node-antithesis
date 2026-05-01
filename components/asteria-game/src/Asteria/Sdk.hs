{- |
Module      : Asteria.Sdk
Description : Antithesis fallback SDK JSONL emitters.

Mirrors the shell helpers in
@components\/sidecar\/composer\/convergence\/helper_sdk_lib.sh@ and the
Haskell wrapper in
@components\/tracer-sidecar\/src\/Cardano\/Antithesis\/Sdk.hs@.

Writes JSON lines to @$ANTITHESIS_OUTPUT_DIR\/sdk.jsonl@ (default
@\/tmp\/sdk.jsonl@). Each call appends one event the Antithesis
hypervisor consumes to track which assertions have been hit.

Iteration 1 only uses 'sdkReachable' as a wiring smoke test — once
the asteria-player container starts and emits this event, we know
every layer (Nix image → docker-compose service → composer driver
→ SDK fallback file) is correctly wired.
-}
module Asteria.Sdk (
    sdkReachable,
    sdkUnreachable,
    sdkSometimes,
    sdkAlways,
) where

import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import System.IO (BufferMode (..), IOMode (..), hClose, hFlush, hPutStrLn, hSetBuffering, openFile, stderr)

-- | Resolve the JSONL output path from @ANTITHESIS_OUTPUT_DIR@.
sdkOutputPath :: IO FilePath
sdkOutputPath = do
    mDir <- lookupEnv "ANTITHESIS_OUTPUT_DIR"
    pure $ case mDir of
        Just d | not (null d) -> d <> "/sdk.jsonl"
        _ -> "/tmp/sdk.jsonl"

-- | Append a single JSON event to the SDK fallback file.
appendJsonl :: Value -> IO ()
appendJsonl v = do
    path <- sdkOutputPath
    createDirectoryIfMissing True (takeDirectory path)
    h <- openFile path AppendMode
    hSetBuffering h LineBuffering
    LBS.hPutStr h (encode v)
    LBS.hPutStr h "\n"
    hFlush h
    hClose h

mkAssert ::
    -- | display_type: reachable | unreachable | sometimes | always
    Text ->
    -- | hit
    Bool ->
    -- | must_hit
    Bool ->
    -- | identifier
    Text ->
    -- | message
    Text ->
    -- | optional details JSON
    Maybe Value ->
    Value
mkAssert displayType hit mustHit ident msg details =
    object
        [ Key.fromText "antithesis_assert"
            .= object
                [ "id" .= ident
                , "message" .= msg
                , "condition" .= hit
                , "display_type" .= displayType
                , "hit" .= hit
                , "must_hit" .= mustHit
                , "assert_type" .= displayType
                , "location"
                    .= object
                        [ "file" .= ("asteria-player" :: Text)
                        , "function" .= ident
                        , "class" .= ("Asteria.Sdk" :: Text)
                        , "begin_line" .= (0 :: Int)
                        , "begin_column" .= (0 :: Int)
                        ]
                , "details" .= fromMaybe (object []) details
                ]
        ]

emit ::
    Text -> Bool -> Bool -> Text -> Text -> Maybe Value -> IO ()
emit displayType hit mustHit ident msg details = do
    let v = mkAssert displayType hit mustHit ident msg details
    appendJsonl v
    -- Also echo to stderr for local docker-compose visibility.
    hPutStrLn stderr $ T.unpack ("[sdk:" <> displayType <> "] " <> ident)

{- | Emit a "reachable" event — fired the first time a code path is
exercised.
-}
sdkReachable :: Text -> Maybe Value -> IO ()
sdkReachable ident = emit "reachable" True True ident ident

{- | Emit an "unreachable" event — for failure paths that should
never fire in a healthy run.
-}
sdkUnreachable :: Text -> Maybe Value -> IO ()
sdkUnreachable ident = emit "unreachable" True True ident ident

{- | Emit a conditional "sometimes" event — Antithesis tracks
whether any run hit the truthy branch.
-}
sdkSometimes :: Bool -> Text -> Maybe Value -> IO ()
sdkSometimes hit ident = emit "sometimes" hit True ident ident

{- | Emit an invariant "always" event — every emission must have
@hit = True@ in a healthy run.
-}
sdkAlways :: Bool -> Text -> Maybe Value -> IO ()
sdkAlways hit ident = emit "always" hit True ident ident
