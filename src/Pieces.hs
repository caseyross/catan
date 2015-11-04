module Pieces where

import qualified Data.IntMap as IntMap

--

data Player = Player    { name      :: String
                        , hand      :: Resources
                        , stockpile :: Stockpile
                        , vp        :: Int
                        } deriving (Eq)

--

newPlayer :: String -> Player
newPlayer name = Player name initHand initStockpile initVP

gainR :: Resources -> Player -> Player
gainR gain pl = pl {hand = tadd5 gain (hand pl)}

spendR :: Resources -> Player -> Player
spendR cost = gainR (tmap5 negate cost)

hasR :: Resources -> Player -> Bool
hasR res pl = tmap5 (>=0) (tadd5 (hand pl) (tmap5 negate res)) == (True, True, True, True, True)

initHand        = (0, 0, 0, 0, 0)
initStockpile   = (15, 5, 4)
initVP          = 0

--

canBuyRoad = hasR roadCost
canBuySettlement = hasR settlementCost
canBuyCity = hasR cityCost

buyRoad = spendR roadCost
buySettlement = spendR settlementCost
buyCity = spendR cityCost

roadCost        = (0, 0, 1, 1, 0)
settlementCost  = (1, 1, 1, 1, 0)
cityCost        = (2, 0, 0, 0, 3)
devCardCost     = (1, 1, 0, 0, 1)

--

type Grain = Int
type Wool = Int
type Lumber = Int
type Brick = Int
type Ore = Int

type Resources = (Grain, Wool, Lumber, Brick, Ore)

type SpareRoads = Int
type SpareSettlements = Int
type SpareCities = Int

type Stockpile = (SpareRoads, SpareSettlements, SpareCities)

--

hexIDs = [-14, -11, -10, -8, -7, -6, -4, -3, -1, 0, 1, 3, 4, 6, 7, 8, 10, 11, 14] :: [Int]

waterHexIDs = [-21, -18, -17, -15, -13, -12, -9, -5, -2, 2, 5, 9, 12, 13, 15, 17, 18, 21] :: [Int]

markers = 2 : 12 : concatMap (replicate 2) ([3..6] ++ [8..11]) :: [Int]

terrains = concat $ map (replicate 4) [Pasture, Plains, Forest] ++ map (replicate 3) [Hills, Mountains]

ports = [WoolPort, GrainPort, LumberPort, BrickPort, OrePort] ++ replicate 4 GenericPort

--

data Hex = Hex  { terrain       :: Terrain
                , productionNum :: Int
                , robber        :: Bool
                , edges         :: IntMap.IntMap Edge
                , corners       :: IntMap.IntMap Corner
                } deriving (Eq)

data Terrain    = Pasture
                | Plains
                | Forest
                | Hills
                | Mountains
                | Desert
                | Water
                deriving (Eq, Show)

data Edge   = EmptyEdge
            | Road Player deriving (Eq)

data Corner = Corner    { status    :: CornerType
                        , port      :: PortType
                        } deriving (Eq)

data CornerType = EmptyCorner
                | Settlement Player
                | City Player
                deriving (Eq)

data PortType   = NoPort
                | WoolPort
                | GrainPort
                | LumberPort
                | BrickPort
                | OrePort
                | GenericPort
                deriving (Eq)

--

tmap3 :: (a -> b) -> (a, a, a) -> (b, b, b)
tmap3 f (a1, a2, a3) = (f a1, f a2, f a3)

tmap5 :: (a -> b) -> (a, a, a, a, a) -> (b, b, b, b, b)
tmap5 f (a1, a2, a3, a4, a5) = (f a1, f a2, f a3, f a4, f a5)

tadd3 :: (Num a) => (a, a, a) -> (a, a, a) -> (a, a, a)
tadd3 (a1, a2, a3) (b1, b2, b3) = (a1 + b1, a2 + b2, a3 + b3)

tadd5 :: (Num a) => (a, a, a, a, a) -> (a, a, a, a, a) -> (a, a, a, a, a)
tadd5 (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5) = (a1 + b1, a2 + b2, a3 + b3, a4 + b4, a5 + b5)