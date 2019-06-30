module Command.Movement where

import Control.Monad (unless, when)
import Control.Monad.Trans.RWS.Strict
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Map ((!))
import Element
import Game
import Location

data Command
  = North
  | South
  | East
  | West
  | Up
  | Down

parse :: [String] -> Maybe Command
parse ["go", x]
  | x == "north" = Just North
  | x == "south" = Just South
  | x == "east" = Just East
  | x == "west" = Just West
  | x == "up" = Just Up
  | x == "down" = Just Down
  | otherwise = Nothing
parse _ = Nothing

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

moveC :: Command -> (Int, Int, Int) -> (Int, Int, Int)
moveC North (x, y, z) = (x, y + 1, z)
moveC South (x, y, z) = (x, y - 1, z)
moveC East (x, y, z) = (x + 1, y, z)
moveC West (x, y, z) = (x - 1, y, z)
moveC Up (x, y, z) = (x, y, z + 1)
moveC Down (x, y, z) = (x, y, z - 1)

moveL :: Command -> House -> Maybe House
moveL c x =
  Bimap.lookupR x positions >>= Just . moveC c >>= flip Bimap.lookup positions

run :: Command -> GameEnv ()
run c = do
  (Game p v m eM e) <- get
  let storageCheck = eM ! storageKey == Location.Bag
      exitCheck = eM ! exitKey == Location.Bag
      visibilityCheck = v
      successRun nL = do
        put $ Game nL v m eM e
        tellN . show $ nL
      runStorageRoom nL = do
        when storageCheck $ successRun nL
        unless storageCheck $ tellN "Oops, You don't have the storage key"
  unless visibilityCheck $
    tellN "Can't move when hidden, Leave the hiding place first"
  case (p, moveL c p) of
    (_, Nothing) -> tellN "Moving out of bounds. Can't move here."
    (StorageRoom, Just nL) -> runStorageRoom nL
    (_, Just StorageRoom) -> runStorageRoom StorageRoom
    (_, Just Exit) -> do
      when exitCheck $ successRun Exit
      unless exitCheck $ tellN "Oops, You don't have the exit key"
    (_, Just nL) -> successRun nL
