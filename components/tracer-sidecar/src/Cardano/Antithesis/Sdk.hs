{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Cardano.Antithesis.Sdk
    ( writeSdkJsonl
    , sometimesTracesDeclaration
    , sometimesFailed
    , sometimesTracesReached
    , alwaysOrUnreachableDeclaration
    , alwaysOrUnreachableFailed
    )
where

import qualified Data.ByteString.Lazy as BL

import Data.Aeson
    ( Value
    , encode
    )
import Data.Aeson.QQ
    ( aesonQQ
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Text
    ( Text
    )
import System.Environment
    ( lookupEnv
    )
import System.IO
    ( IOMode (AppendMode)
    , withFile
    )

-- | Append a JSON Value as one line to $ANTITHESIS_OUTPUT_DIR/sdk.jsonl
writeSdkJsonl :: Value -> IO ()
writeSdkJsonl v = do
    dir <- fromMaybe "/tmp" <$> lookupEnv "ANTITHESIS_OUTPUT_DIR"
    let outFile = dir ++ "/sdk.jsonl"
    -- open in AppendMode and write the JSON + newline
    withFile outFile AppendMode $ \h ->
        BL.hPutStr h (encode v <> "\n")

-- Hard code values for now

sometimesTracesDeclaration :: Text -> Value
sometimesTracesDeclaration label =
    [aesonQQ|
    {
      "antithesis_assert": {
        "id":           #{label},
        "message":      #{label},
        "condition":    false,
        "display_type": "Sometimes",
        "hit":          false,
        "must_hit":     true,
        "assert_type":  "sometimes",
        "location": #{dummyLocation},
        "details": null
      }
    }
    |]

-- FIXME: Too much boilerplate, should probably add Haskell types now
sometimesFailed :: Text -> Value -> Value
sometimesFailed label details =
    [aesonQQ|
    {
      "antithesis_assert": {
        "id":           #{label},
        "message":      #{label},
        "condition":    false,
        "display_type": "Sometimes",
        "hit":          true,
        "must_hit":     true,
        "assert_type":  "sometimes",
        "location": #{dummyLocation},
        "details": #{details}
      }
    }
    |]

sometimesTracesReached :: Text -> Value
sometimesTracesReached label =
    [aesonQQ|
    {
      "antithesis_assert": {
        "id":           #{label},
        "message":      #{label},
        "condition":    true,
        "display_type": "Sometimes",
        "hit":          true,
        "must_hit":     true,
        "assert_type":  "sometimes",
        "location": #{dummyLocation},
        "details": null
      }
    }
    |]

alwaysOrUnreachableDeclaration :: Text -> Value
alwaysOrUnreachableDeclaration label =
    [aesonQQ|
    {
      "antithesis_assert": {
        "id":           #{label},
        "message":      #{label},
        "condition":    false,
        "display_type": "AlwaysOrUnreachable",
        "hit":          false,
        "must_hit":     false,
        "assert_type":  "always",
        "location": #{dummyLocation},
        "details": null
      }
    }
    |]

alwaysOrUnreachableFailed :: Text -> Value -> Value
alwaysOrUnreachableFailed label details =
    [aesonQQ|
    {
      "antithesis_assert": {
        "id":           #{label},
        "message":      #{label},
        "condition":    false,
        "display_type": "AlwaysOrUnreachable",
        "hit":          true,
        "must_hit":     false,
        "assert_type":  "always",
        "location": #{dummyLocation},
        "details": #{details}
      }
    }
    |]

_alwaysDeclaration :: Text -> Value
_alwaysDeclaration label =
    [aesonQQ|
    {
      "antithesis_assert": {
        "id":           #{label},
        "message":      #{label},
        "condition":    false,
        "display_type": "Always",
        "hit":          false,
        "must_hit":     true,
        "assert_type":  "always",
        "location": #{dummyLocation},
        "details": null
      }
    }
    |]

_alwaysReached :: Text -> Value -> Value
_alwaysReached label details =
    [aesonQQ|
    {
      "antithesis_assert": {
        "id":           #{label},
        "message":      #{label},
        "condition":    true,
        "display_type": "Always",
        "hit":          true,
        "must_hit":     true,
        "assert_type":  "always",
        "location": #{dummyLocation},
        "details": #{details}
      }
    }
    |]

dummyLocation :: Value
dummyLocation =
    [aesonQQ|
    {
          "file":         "",
          "function":     "",
          "class":        "",
          "begin_line":   0,
          "begin_column": 0
    } |]
