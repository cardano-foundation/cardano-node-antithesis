{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Asteria.Validators
Description : Load the four parameter-applied asteria validators.

Reads the parameter-applied blueprint at
@aiken\/plutus-applied.json@ at compile time (via 'embedFile') and
exposes each validator's compiled bytes as a 'Script' 'ConwayEra'.

The parameter values baked in are documented in
@aiken\/apply-params.sh@:

  * @admin_token@ = AssetClass with policy @00..00@ (28 zero
    bytes) and asset name @"asteriaAdmin"@.
  * Game constants: @ship_mint_lovelace_fee = 3_000_000@,
    @max_asteria_mining = 50@, @min_asteria_distance = 50@,
    @initial_fuel = 100@, @max_speed = Speed 1 30000@,
    @max_ship_fuel = 100@, @fuel_per_step = 5@.

Iteration 5 will use bootstrap-time data to recompute these values
(in particular, @admin_token@ should be a one-shot mint tied to a
specific @TxOutRef@). For now they're hard-wired so the validators
are usable as-is on devnet.
-}
module Asteria.Validators (
    asteriaScript,
    spacetimeScript,
    pelletScript,
    deployScript,
    adminMintScript,
    Validator (..),
    appliedBlueprint,
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
blueprintBytes = $(embedFile "aiken/plutus-applied.json")

{- | Decoded blueprint. Throws at first use if the JSON has drifted
from the schema; that's a build-time invariant we want loud.
-}
appliedBlueprint :: Blueprint
appliedBlueprint =
    case eitherDecodeStrict blueprintBytes of
        Left e -> error ("Asteria.Validators: blueprint decode failed: " <> e)
        Right b -> b

-- | Look up a validator by exact title (e.g. @asteria.asteria.spend@).
findValidator :: Text -> Validator
findValidator title =
    case find (\v -> vTitle v == title) (bpValidators appliedBlueprint) of
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

-- | Always-true mint policy whose hash is baked into 'admin_token'.
adminMintScript :: Script ConwayEra
adminMintScript = mkScript (vCompiledCode (findValidator "admin_mint.admin_mint.mint"))
