module Game 
  ( module Game
  , module Asset
  ) where

import Control.Monad.Trans.RWS.Strict
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Asset

data Game = Game
  { player :: House
  , visibility :: Bool
  , murderer :: House
  , elementMap :: Map Element Location
  , electricity :: Bool
  } deriving (Show)

type GameEnv = RWST () String Game IO

initialPlayerLocation = LivingArea

initialMurdererLocation = ChemistryLab

initialVisibility = True

initialElectricity = False

initialElementLocation =
  Map.fromList
    [ (ExitKey, None)
    , (Mold, None)
    , (StorageKey, H HomeTheater)
    , (Table Red, H PlayRoom)
    , (Table Red, H TreeHouseFirstFloor)
    , (Table Blue, H MasterBedroom)
    , (Table Green, H Kitchen)
    , (Cabinet Red, H SmokingArea)
    , (Cabinet Green, H HomeTheater)
    , (Cabinet Blue, H MasterBedroom)
    , (Generator, H GeneratorRoom)
    , (Furnace, H FireRoom)
    , (Oil, H Garage)
    , (Lighter, H BalconyThirdFloor)
    , (Steel, H StorageRoom)
    , (GasZ, H Pool)
    , (Clay, H Garage)
    , (ChemicalChamber, H ChemistryLab)
    ]

initialGame =
  Game
    initialPlayerLocation
    initialVisibility
    initialMurdererLocation
    initialElementLocation
    initialElectricity

isAvailable :: Element -> Game -> Bool
isAvailable x (Game p _ _ eM _) =
  (eM ! x == H p) || (eM ! x == Bag) 

tellN w = tell "\n" >> tell w
