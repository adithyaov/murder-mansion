module Game.Map where

import Asset
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

positions :: Bimap (Int, Int, Int) House
positions =
  Bimap.fromList
    [ ((0, 0, 0), LivingArea)
    , ((0, 1, 0), Pool)
    , ((1, 0, 0), TreeHouseGroundFloor)
    , ((1, 0, 1), TreeHouseFirstFloor)
    , ((-1, 0, 0), FireRoom)
    , ((0, 0, -1), Garage)
    , ((0, 0, 1), HallWayFirstFloor)
    , ((-1, 0, 1), MasterBedroom)
    , ((-1, 1, 1), BathroomMasterBedroom)
    , ((0, 1, 1), DiningHall)
    , ((1, 0, 1), PlayRoom)
    , ((1, 1, 1), BathroomPlayRoom)
    , ((0, 0, 2), HallWaySecondFloor)
    , ((-1, 0, 2), SmokingArea)
    , ((1, 0, 2), HomeTheater)
    , ((0, 0, 3), HallWayThirdFloor)
    , ((0, 1, 3), StorageRoom)
    , ((1, 0, 3), Kitchen)
    , ((2, 0, 3), KitchenStorage)
    , ((-1, 0, 3), BalconyThirdFloor)
    , ((0, 0, 4), HallwayFourthFloor)
    , ((1, 0, 4), GeneratorRoom)
    , ((1, 1, 4), ChemistryLab)
    , ((0, -1, 0), Exit)
    ]

