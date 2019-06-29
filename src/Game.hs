module Game where

import Control.Monad.Trans.RWS.Strict
import Data.Map (Map)
import qualified Data.Map as Map
import Element
import Location

data Game = Game
  { player :: House
  , visibility :: Bool
  , murderer :: House
  , elementMap :: Map Element Location
  , electricity :: Bool
  }

type GameEnv = RWS () String Game

initialElementLocation = Map.fromList
  [ (ExitKey, None)
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
  , (ChemicalChamber,  H ChemistryLab) ]

tellN w = tell "\n" >> tell w
