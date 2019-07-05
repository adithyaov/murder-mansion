-- This is the internal game module
module Game.Internal
  ( module Game.Internal
  , module Asset
  ) where

import Asset
import Control.Monad.Trans.RWS.Strict
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Bimap (Bimap)

-- This is the game state.
data Game = Game
  { player :: House
  , visibility :: Bool
  , murderer :: House
  , elementMap :: Map Element Location
  , electricity :: Bool
  } deriving (Show, Eq)

-- The environment of the game state is a standard RWS monad over IO.
type GameEnv = RWST () String Game IO

-- The type of a map in the game
type GameMap = Bimap (Int, Int, Int) House

-- type of recipe map
type RecipeMap = Map Element [Either Element Element]

-- type of some entry requirement encoding
type EntryRequirementMap = Map House [Element]

initialPlayerLocation = LivingArea

initialMurdererLocation = ChemistryLab

initialVisibility = True

initialElectricity = False

-- Recipies of elements
recipies =
  Map.fromList
    [ (ExitKey, [Left Mold, Left Steel, Right Furnace, Left Oil, Left Lighter])
    , (Mold, [Right ChemicalChamber, Left GasZ, Left Clay]) ]

-- Entry requirements
entryRequirements =
  Map.fromList
    [ (Exit, [ExitKey])
    , (StorageRoom, [StorageKey]) ]

-- The initial location of all the elements.
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

-- A simple functions to check of resources are available in the vicinity.
isAvailable :: Element -> Game -> Bool
isAvailable x (Game p _ _ eM _) = (eM ! x == H p) || (eM ! x == Bag)

-- This function describes all the elements in the house.
describe :: House -> GameEnv ()
describe r = do
  (Game _ _ _ eM _) <- get
  let elmFind k y a
        | y == H r = k : a
        | otherwise = a
      desc x = fromAsset x ++ ": " ++ info x
  sequence_ $ mytell . desc <$> Map.foldrWithKey elmFind [] eM

-- A simple helper function over tell.
mytell w = tell w >> tell "\n" 

-- A simple function to only get Left elements
leftElements :: [Either a b] -> [a]
leftElements xs = [ x | Left x <- xs ]

-- A simple function to get all elements
allElements :: [Either a a] -> [a]
allElements xs = [ x | Left x <- xs ] ++ [ x | Right x <- xs ]

commandPrepend = ">> "
