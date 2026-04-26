{- |
Module      : Asteria.Datums
Description : Aiken-shaped datum / redeemer ADTs.

Mirrors @lib\/asteria\/types.ak@ from the vendored asteria source.
'ToData' / 'FromData' instances match Aiken's record encoding
(Constr 0 [field0, field1, ...]) and the @ShipRedeemer@ /
@AsteriaRedeemer@ / @PelletRedeemer@ enum encodings (Constr i [args]).
-}
module Asteria.Datums (
    AsteriaDatum (..),
    ShipDatum (..),
    PelletDatum (..),
    AsteriaRedeemer (..),
    ShipRedeemer (..),
    PelletRedeemer (..),
) where

import PlutusCore.Data (Data (..))
import PlutusTx.Builtins.Internal (BuiltinByteString (..), BuiltinData (..))
import PlutusTx.IsData.Class (FromData (..), ToData (..))

-- | @AsteriaDatum { ship_counter: Int, shipyard_policy: PolicyId }@
data AsteriaDatum = AsteriaDatum
    { adShipCounter :: Integer
    , adShipyardPolicy :: BuiltinByteString
    }
    deriving stock (Eq, Show)

instance ToData AsteriaDatum where
    toBuiltinData AsteriaDatum{adShipCounter = c, adShipyardPolicy = p} =
        BuiltinData $
            Constr
                0
                [ I c
                , B (let BuiltinByteString bs = p in bs)
                ]

instance FromData AsteriaDatum where
    fromBuiltinData (BuiltinData d) = case d of
        Constr 0 [I c, B p] ->
            Just (AsteriaDatum c (BuiltinByteString p))
        _ -> Nothing

{- | @ShipDatum { pos_x, pos_y, ship_token_name, pilot_token_name,
last_move_latest_time }@
-}
data ShipDatum = ShipDatum
    { sdPosX :: Integer
    , sdPosY :: Integer
    , sdShipTokenName :: BuiltinByteString
    , sdPilotTokenName :: BuiltinByteString
    , sdLastMoveLatestTime :: Integer
    }
    deriving stock (Eq, Show)

instance ToData ShipDatum where
    toBuiltinData
        ShipDatum
            { sdPosX = x
            , sdPosY = y
            , sdShipTokenName = s
            , sdPilotTokenName = p
            , sdLastMoveLatestTime = t
            } =
            BuiltinData $
                Constr
                    0
                    [ I x
                    , I y
                    , B (let BuiltinByteString bs = s in bs)
                    , B (let BuiltinByteString bs = p in bs)
                    , I t
                    ]

instance FromData ShipDatum where
    fromBuiltinData (BuiltinData d) = case d of
        Constr 0 [I x, I y, B s, B p, I t] ->
            Just
                ( ShipDatum
                    x
                    y
                    (BuiltinByteString s)
                    (BuiltinByteString p)
                    t
                )
        _ -> Nothing

-- | @PelletDatum { pos_x, pos_y, shipyard_policy }@
data PelletDatum = PelletDatum
    { pdPosX :: Integer
    , pdPosY :: Integer
    , pdShipyardPolicy :: BuiltinByteString
    }
    deriving stock (Eq, Show)

instance ToData PelletDatum where
    toBuiltinData
        PelletDatum
            { pdPosX = x
            , pdPosY = y
            , pdShipyardPolicy = s
            } =
            BuiltinData $
                Constr
                    0
                    [ I x
                    , I y
                    , B (let BuiltinByteString bs = s in bs)
                    ]

instance FromData PelletDatum where
    fromBuiltinData (BuiltinData d) = case d of
        Constr 0 [I x, I y, B s] ->
            Just (PelletDatum x y (BuiltinByteString s))
        _ -> Nothing

-- | @AsteriaRedeemer = AddNewShip | Mine | ConsumeAsteria@
data AsteriaRedeemer = AddNewShip | Mine | ConsumeAsteria
    deriving stock (Eq, Show)

instance ToData AsteriaRedeemer where
    toBuiltinData AddNewShip = BuiltinData (Constr 0 [])
    toBuiltinData Mine = BuiltinData (Constr 1 [])
    toBuiltinData ConsumeAsteria = BuiltinData (Constr 2 [])

{- | @ShipRedeemer = MoveShip {dx,dy} | GatherFuel {amount} |
MineAsteria | Quit@
-}
data ShipRedeemer
    = MoveShip Integer Integer
    | GatherFuel Integer
    | MineAsteria
    | Quit
    deriving stock (Eq, Show)

instance ToData ShipRedeemer where
    toBuiltinData (MoveShip x y) = BuiltinData (Constr 0 [I x, I y])
    toBuiltinData (GatherFuel n) = BuiltinData (Constr 1 [I n])
    toBuiltinData MineAsteria = BuiltinData (Constr 2 [])
    toBuiltinData Quit = BuiltinData (Constr 3 [])

-- | @PelletRedeemer = Provide {amount} | ConsumePellet@
data PelletRedeemer = Provide Integer | ConsumePellet
    deriving stock (Eq, Show)

instance ToData PelletRedeemer where
    toBuiltinData (Provide n) = BuiltinData (Constr 0 [I n])
    toBuiltinData ConsumePellet = BuiltinData (Constr 1 [])
