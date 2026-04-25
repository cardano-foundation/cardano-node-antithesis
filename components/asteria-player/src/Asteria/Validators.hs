{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Asteria.Validators
Description : Load the four asteria Aiken validators from plutus.json.

Reads the blueprint at @aiken\/plutus.json@ at compile time (via
'embedFile') and exposes each validator's compiled bytes as a
'Script' 'ConwayEra'.

Iteration 3 ships the validators *unapplied* — they're still
parameterized and not usable as-is. Iteration 4 will add a thin
Aiken-side parameter-application script (admin token, game
constants, cross-validator hashes) and replace 'unappliedBlueprint'
with the applied bytes.

Until then, these helpers exist so:

  - The blueprint is committed and reviewable.
  - The Haskell loading machinery is proven.
  - Iteration 4 only has to swap the JSON file and add the apply
    derivation; the player code can already reference each
    validator by name.
-}
module Asteria.Validators (
    asteriaScript,
    spacetimeScript,
    pelletScript,
    deployScript,
    Validator (..),
    unappliedBlueprint,
) where

import Cardano.Ledger.Alonzo.Scripts (fromPlutusScript, mkPlutusScript)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (Script)
import Cardano.Ledger.Plutus.Language (
    Language (PlutusV3),
    Plutus (..),
    PlutusBinary (..),
 )
import Data.Aeson (FromJSON, eitherDecodeStrict, withObject, (.:))
import Data.Aeson.Types (parseJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Short qualified as SBS
import Data.FileEmbed (embedFile)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- | One entry from @plutus.json@'s @validators@ array.
data Validator = Validator
    { vTitle :: Text
    , vCompiledCode :: Text
    , vHash :: Text
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON Validator where
    parseJSON = withObject "Validator" $ \o ->
        Validator
            <$> o .: "title"
            <*> o .: "compiledCode"
            <*> o .: "hash"

-- | Internal blueprint shape: @{ "validators": [...] }@.
newtype Blueprint = Blueprint {bpValidators :: [Validator]}
    deriving stock (Eq, Show, Generic)

instance FromJSON Blueprint where
    parseJSON = withObject "Blueprint" $ \o ->
        Blueprint <$> o .: "validators"

-- | Raw bytes of the blueprint, embedded at build time.
blueprintBytes :: ByteString
blueprintBytes = $(embedFile "aiken/plutus.json")

{- | Decoded blueprint. Throws at first use if the JSON has drifted
from the schema; that's a build-time invariant we want loud.
-}
unappliedBlueprint :: Blueprint
unappliedBlueprint =
    case eitherDecodeStrict blueprintBytes of
        Left e -> error ("Asteria.Validators: blueprint decode failed: " <> e)
        Right b -> b

-- | Look up a validator by exact title (e.g. @asteria.asteria.spend@).
findValidator :: Text -> Validator
findValidator title =
    case find (\v -> vTitle v == title) (bpValidators unappliedBlueprint) of
        Just v -> v
        Nothing ->
            error
                ( "Asteria.Validators: no validator titled "
                    <> T.unpack title
                    <> " in blueprint"
                )

{- | Decode a hex-encoded compiled-code field into a Conway-era
Plutus V3 'Script'.
-}
mkScript :: Text -> Script ConwayEra
mkScript hex =
    let bs =
            either
                (error . ("Asteria.Validators: hex decode: " <>))
                id
                (Base16.decode (BS8.pack (T.unpack hex)))
        plutus = Plutus @PlutusV3 (PlutusBinary (SBS.toShort bs))
     in case mkPlutusScript plutus of
            Just ps -> fromPlutusScript ps
            Nothing -> error "Asteria.Validators: mkPlutusScript failed"

asteriaScript :: Script ConwayEra
asteriaScript = mkScript (vCompiledCode (findValidator "asteria.asteria.spend"))

spacetimeScript :: Script ConwayEra
spacetimeScript = mkScript (vCompiledCode (findValidator "spacetime.spacetime.spend"))

pelletScript :: Script ConwayEra
pelletScript = mkScript (vCompiledCode (findValidator "pellet.pellet.spend"))

deployScript :: Script ConwayEra
deployScript = mkScript (vCompiledCode (findValidator "deploy.deploy.spend"))
