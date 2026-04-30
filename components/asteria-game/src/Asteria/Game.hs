{- |
Module      : Asteria.Game
Description : Game-action TxBuild programs.

Each function takes the current world state plus relevant
addresses and returns a 'TxBuild' program that — once built,
signed, and submitted — advances the game by one step.

Iteration 6b ships only 'spawnShipProgram'. Subsequent iterations
will add 'moveShipProgram', 'gatherFuelProgram', 'mineAsteriaProgram',
and 'quitProgram'.
-}
module Asteria.Game (
    SpawnShipParams (..),
    spawnShipProgram,
    spawnShipPosX,
    spawnShipPosY,
    initialFuel,
    shipMintLovelaceFee,
    shipUtxoMinAda,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Short qualified as SBS
import Data.Map.Strict qualified as Map
import Data.Void (Void)
import Data.Word (Word32)
import Lens.Micro ((^.))

import Cardano.Ledger.Address (Addr)
import Cardano.Ledger.Api.Tx.Out (TxOut, valueTxOutL)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (Script)
import Cardano.Ledger.Mary.Value (
    AssetName (..),
    MaryValue (..),
    MultiAsset (..),
    PolicyID (..),
 )
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Node.Client.TxBuild (
    TxBuild,
    attachScript,
    collateral,
    mint,
    payTo',
    spend,
    spendScript,
    validTo,
 )
import Cardano.Slotting.Slot (SlotNo (..))
import PlutusTx.Builtins.Internal (BuiltinByteString (..))

import Asteria.Datums (
    AsteriaDatum (..),
    AsteriaRedeemer (..),
    FuelRedeemer (..),
    ShipDatum (..),
    ShipyardRedeemer (..),
 )

-- | All state the spawn-ship tx needs.
data SpawnShipParams = SpawnShipParams
    { sspAsteriaIn :: TxIn
    , sspAsteriaOut :: TxOut ConwayEra
    , sspAsteriaAddr :: Addr
    , sspShipAddr :: Addr
    , sspAsteriaDatum :: AsteriaDatum
    , sspAdminPolicy :: PolicyID
    , sspAdminName :: AssetName
    , sspShipyardPolicy :: PolicyID
    , sspFuelPolicy :: PolicyID
    , sspAsteriaScript :: Script ConwayEra
    , sspSpacetimeScript :: Script ConwayEra
    , sspPelletScript :: Script ConwayEra
    , sspFundingIn :: TxIn
    , sspValidTo :: SlotNo
    }

{- | Hard-coded spawn position. Asteria's add_new_ship rule
requires distance from origin ≥ min_asteria_distance (50);
(50, 0) sits right on that boundary.
-}
spawnShipPosX, spawnShipPosY :: Integer
spawnShipPosX = 50
spawnShipPosY = 0

-- | Iter-4 game constants. Must mirror @apply-params.sh@.
initialFuel, shipMintLovelaceFee, shipUtxoMinAda :: Integer
initialFuel = 100
shipMintLovelaceFee = 3_000_000
shipUtxoMinAda = 2_500_000

shipName, pilotName :: Integer -> AssetName
shipName n = AssetName (SBS.toShort (BS8.pack ("SHIP" <> show n)))
pilotName n = AssetName (SBS.toShort (BS8.pack ("PILOT" <> show n)))

unwrapAssetName :: AssetName -> BS.ByteString
unwrapAssetName (AssetName sbs) = SBS.fromShort sbs

-- ---------------------------------------------------------------
-- spawnShip
-- ---------------------------------------------------------------

spawnShipProgram :: SpawnShipParams -> TxBuild q Void Word32
spawnShipProgram SpawnShipParams{..} = do
    let counter = adShipCounter sspAsteriaDatum
        nextCounter = counter + 1
        sName = shipName counter
        pName = pilotName counter
        fuelName = AssetName (SBS.toShort (BS8.pack "FUEL"))
        asteriaDatum' =
            sspAsteriaDatum{adShipCounter = nextCounter}
        -- Asteria output value = asteria input value + ship_mint_lovelace_fee
        MaryValue (Coin coinIn) maIn = sspAsteriaOut ^. valueTxOutL
        asteriaOutValue =
            MaryValue (Coin (coinIn + shipMintLovelaceFee)) maIn
        shipOutValue =
            MaryValue (Coin shipUtxoMinAda) $
                MultiAsset $
                    Map.fromList
                        [ (sspShipyardPolicy, Map.singleton sName 1)
                        , (sspFuelPolicy, Map.singleton fuelName initialFuel)
                        ]
        shipDatum =
            ShipDatum
                { sdPosX = spawnShipPosX
                , sdPosY = spawnShipPosY
                , sdShipTokenName = BuiltinByteString (unwrapAssetName sName)
                , sdPilotTokenName = BuiltinByteString (unwrapAssetName pName)
                , -- The asteria validator compares
                  -- @last_move_latest_time >= tx_latest_time@
                  -- where @tx_latest_time@ is the validity-range
                  -- upper bound translated to POSIX milliseconds.
                  -- The slot→POSIX mapping isn't available at
                  -- build time, so we set a far-future value
                  -- (year 2286) that always wins the comparison.
                  -- Iter 6c will tighten this once we read the
                  -- system-start time from the genesis config.
                  sdLastMoveLatestTime = 9_999_999_999_999
                }
    _ <- spendScript sspAsteriaIn AddNewShip
    _ <- spend sspFundingIn
    collateral sspFundingIn
    attachScript sspAsteriaScript
    attachScript sspSpacetimeScript
    attachScript sspPelletScript
    -- Spacetime mint: 1 SHIP_n + 1 PILOT_n via MintShip redeemer.
    mint
        sspShipyardPolicy
        (Map.fromList [(sName, 1), (pName, 1)])
        MintShip
    -- Pellet mint: initial_fuel FUEL via MintFuel redeemer.
    mint sspFuelPolicy (Map.singleton fuelName initialFuel) MintFuel
    validTo sspValidTo
    -- The PILOT NFT is left for the balancer to fold
    -- into the player's change output, matching the
    -- 3-output spawn-tx shape used on mainnet. This
    -- relies on the residual-multi-asset folding
    -- added in cardano-node-clients balanceTx; see
    -- lambdasistemi/cardano-node-clients PR #77.
    _ <- payTo' sspAsteriaAddr asteriaOutValue asteriaDatum'
    payTo' sspShipAddr shipOutValue shipDatum
