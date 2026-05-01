{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Asteria.Validators
Description : Apply per-deploy parameters to the asteria blueprint.

The Aiken blueprint @aiken\/plutus.json@ ships the validators
\"un-applied\" — each validator declares parameters that must be
substituted before the script can be hashed and submitted on
chain. This module embeds the blueprint at compile time and
exposes 'applyScripts' which substitutes the per-deploy seed
'TxIn' (plus the fixed game constants) into all four validators
at runtime.

The seed-shifted policy hash is the on-chain enforcement of the
admin-singleton invariant: 'admin_mint' is parameterised on the
seed, so its hash differs per deploy and \"asteriaAdmin\" can be
minted at most once across all chain history (the seed
'OutputReference' can only ever be consumed once).

Game constants are baked in at the Haskell level so changing them
is a pure code change — no shell script or @aiken blueprint apply@
indirection.
-}
module Asteria.Validators (
    -- * Per-deploy applied scripts
    AppliedScripts (..),
    applyScripts,

    -- * Game constants (Plutus parameters baked in)
    shipMintLovelaceFee,
    maxAsteriaMining,
    minAsteriaDistance,
    initialFuel,
    maxSpeedDistance,
    maxSpeedTime,
    maxShipFuel,
    fuelPerStep,
    asteriaAdminAssetName,

    -- * Internal
    Validator (..),
    blueprint,
) where

import Control.Monad.Except (runExcept)
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

import Cardano.Ledger.Alonzo.Scripts (fromPlutusScript, mkPlutusScript)
import Cardano.Ledger.BaseTypes (TxIx (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (Script, hashScript)
import Cardano.Ledger.Hashes (ScriptHash (..), extractHash)
import Cardano.Ledger.Plutus.Language (
    Language (PlutusV3),
    Plutus (..),
    PlutusBinary (..),
 )
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))

import PlutusCore qualified as PLC
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.Common (
    SerialisedScript,
    serialiseUPLC,
    uncheckedDeserialiseUPLC,
 )
import PlutusTx (Data (..))
import UntypedPlutusCore qualified as UPLC

import Asteria.Crypto (hashToBuiltinByteString)
import PlutusTx.Builtins.Internal (BuiltinByteString (..))

-- ---------------------------------------------------------------
-- Game constants
-- ---------------------------------------------------------------

shipMintLovelaceFee, maxAsteriaMining, minAsteriaDistance, initialFuel :: Integer
shipMintLovelaceFee = 3_000_000
maxAsteriaMining = 50
minAsteriaDistance = 50
initialFuel = 100

maxSpeedDistance, maxSpeedTime, maxShipFuel, fuelPerStep :: Integer
maxSpeedDistance = 1
maxSpeedTime = 30_000
maxShipFuel = 100
fuelPerStep = 5

-- | Asset name minted under the per-deploy admin policy.
asteriaAdminAssetName :: ByteString
asteriaAdminAssetName = BS8.pack "asteriaAdmin"

-- ---------------------------------------------------------------
-- Blueprint parsing
-- ---------------------------------------------------------------

data Validator = Validator
    { vTitle :: Text
    , vCompiledCode :: Text
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON Validator where
    parseJSON = withObject "Validator" $ \o ->
        Validator
            <$> o .: "title"
            <*> o .: "compiledCode"

newtype Blueprint = Blueprint {bpValidators :: [Validator]}
    deriving stock (Eq, Show, Generic)

instance FromJSON Blueprint where
    parseJSON = withObject "Blueprint" $ \o ->
        Blueprint <$> o .: "validators"

blueprintBytes :: ByteString
blueprintBytes = $(embedFile "aiken/plutus.json")

blueprint :: Blueprint
blueprint =
    case eitherDecodeStrict blueprintBytes of
        Left e ->
            error
                ("Asteria.Validators: blueprint decode: " <> e)
        Right b -> b

unappliedScript :: Text -> SerialisedScript
unappliedScript title =
    case find (\v -> vTitle v == title) (bpValidators blueprint) of
        Just v ->
            case Base16.decode (BS8.pack (T.unpack (vCompiledCode v))) of
                Right bs -> SBS.toShort bs
                Left e ->
                    error
                        ( "Asteria.Validators: hex decode for "
                            <> T.unpack title
                            <> ": "
                            <> e
                        )
        Nothing ->
            error
                ( "Asteria.Validators: no validator titled "
                    <> T.unpack title
                    <> " in blueprint"
                )

-- ---------------------------------------------------------------
-- Apply-params helpers
-- ---------------------------------------------------------------

dataAsProgram ::
    Data ->
    UPLC.Program UPLC.DeBruijn PLC.DefaultUni PLC.DefaultFun ()
dataAsProgram d =
    UPLC.Program () plcVersion110 (UPLC.Constant () (PLC.someValue d))

applyOne :: SerialisedScript -> Data -> SerialisedScript
applyOne script d =
    let prog = uncheckedDeserialiseUPLC script
        argP = dataAsProgram d
     in case runExcept (UPLC.applyProgram prog argP) of
            Right p -> serialiseUPLC p
            Left err ->
                error
                    ( "Asteria.Validators: applyProgram: "
                        <> show err
                    )

applyAll :: SerialisedScript -> [Data] -> SerialisedScript
applyAll = foldl applyOne

toScript :: SerialisedScript -> Script ConwayEra
toScript bin =
    case mkPlutusScript (Plutus @PlutusV3 (PlutusBinary bin)) of
        Just ps -> fromPlutusScript ps
        Nothing -> error "Asteria.Validators: mkPlutusScript failed"

-- ---------------------------------------------------------------
-- Plutus.Data shapes for parameters
-- ---------------------------------------------------------------

{- | Aiken stdlib's @cardano/transaction.OutputReference@ is a
record @{ transaction_id: ByteArray, output_index: Int }@ — flat
@Constr 0 [B txid, I idx]@ with no @TxId@ wrapper.
-}
seedAsData :: TxIn -> Data
seedAsData (TxIn (TxId h) (TxIx ix)) =
    let BuiltinByteString txidBytes = hashToBuiltinByteString (extractHash h)
     in Constr 0 [B txidBytes, I (fromIntegral ix)]

{- | @AssetClass { policy: PolicyId, name: AssetName }@ as
@Constr 0 [B policy, B name]@.
-}
adminTokenAsData :: ScriptHash -> Data
adminTokenAsData hash_ =
    Constr 0 [B (scriptHashBytes hash_), B asteriaAdminAssetName]

scriptHashBytes :: ScriptHash -> ByteString
scriptHashBytes (ScriptHash h) =
    let BuiltinByteString bs = hashToBuiltinByteString h
     in bs

addressAsData :: ScriptHash -> Data
addressAsData h = B (scriptHashBytes h)

speedAsData :: Integer -> Integer -> Data
speedAsData distance time = Constr 0 [I distance, I time]

intAsData :: Integer -> Data
intAsData = I

-- ---------------------------------------------------------------
-- Per-deploy applied scripts
-- ---------------------------------------------------------------

data AppliedScripts = AppliedScripts
    { asAdminMintScript :: Script ConwayEra
    , asAdminMintHash :: ScriptHash
    , asPelletScript :: Script ConwayEra
    , asPelletHash :: ScriptHash
    , asAsteriaScript :: Script ConwayEra
    , asAsteriaHash :: ScriptHash
    , asSpacetimeScript :: Script ConwayEra
    , asSpacetimeHash :: ScriptHash
    }

{- | Apply the per-deploy seed 'TxIn' to all four validators.

Order matters: @admin_mint@ first (its hash feeds @admin_token@),
then @pellet@ (its hash feeds @asteria@\/@spacetime@), then
@asteria@ (its hash feeds @spacetime@), then @spacetime@.
-}
applyScripts :: TxIn -> AppliedScripts
applyScripts seed =
    let adminMintBin =
            applyOne
                (unappliedScript "admin_mint.admin_mint.mint")
                (seedAsData seed)
        adminMintScr = toScript adminMintBin
        adminMintH = hashScript adminMintScr

        adminToken = adminTokenAsData adminMintH

        pelletBin =
            applyOne
                (unappliedScript "pellet.pellet.spend")
                adminToken
        pelletScr = toScript pelletBin
        pelletH = hashScript pelletScr
        pelletAddr = addressAsData pelletH

        asteriaBin =
            applyAll
                (unappliedScript "asteria.asteria.spend")
                [ pelletAddr
                , adminToken
                , intAsData shipMintLovelaceFee
                , intAsData maxAsteriaMining
                , intAsData minAsteriaDistance
                , intAsData initialFuel
                ]
        asteriaScr = toScript asteriaBin
        asteriaH = hashScript asteriaScr
        asteriaAddr = addressAsData asteriaH

        spacetimeBin =
            applyAll
                (unappliedScript "spacetime.spacetime.spend")
                [ pelletAddr
                , asteriaAddr
                , adminToken
                , speedAsData maxSpeedDistance maxSpeedTime
                , intAsData maxShipFuel
                , intAsData fuelPerStep
                ]
        spacetimeScr = toScript spacetimeBin
        spacetimeH = hashScript spacetimeScr
     in AppliedScripts
            { asAdminMintScript = adminMintScr
            , asAdminMintHash = adminMintH
            , asPelletScript = pelletScr
            , asPelletHash = pelletH
            , asAsteriaScript = asteriaScr
            , asAsteriaHash = asteriaH
            , asSpacetimeScript = spacetimeScr
            , asSpacetimeHash = spacetimeH
            }
