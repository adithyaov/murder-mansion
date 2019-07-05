-- This module defines the movement of a character.
module Command.Movement where

import Control.Monad (unless, when, sequence_)
import Control.Monad.Trans.RWS.Strict
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Map ((!))
import qualified Data.Map as Map
import Game.Internal
import Game.Map

-- Basic directions the player can move.
data Command
  = North
  | South
  | East
  | West
  | Up
  | Down

instance ResponseMessage Command where
  success _ = "successfully moved."
  failuer _ = "failed to move, there is no path here."

-- A simple parser.
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

-- This function takes a command and a location to give out a new location.
moveC :: Command -> (Int, Int, Int) -> (Int, Int, Int)
moveC North (x, y, z) = (x, y + 1, z)
moveC South (x, y, z) = (x, y - 1, z)
moveC East (x, y, z) = (x + 1, y, z)
moveC West (x, y, z) = (x - 1, y, z)
moveC Up (x, y, z) = (x, y, z + 1)
moveC Down (x, y, z) = (x, y, z - 1)

-- This function uses the 'positions' defined in 'Game.Map' to provide a valid new location.
moveL :: GameMap -> House -> Command -> Maybe House
moveL m x c =
  Bimap.lookupR x m >>= Just . moveC c >>= flip Bimap.lookup m

-- Enter is a function which checks requirements prior entry
enter :: GameMap -> EntryRequirementMap -> Command -> GameEnv ()
enter gM eRM c = do
  g@(Game p v m eM e) <- get
  let successRun nL = do
        put $ g { player = nL }
        mytell . success $ c
        mytell $ "you're currently in " ++ fromAsset nL
        mytell . info $ nL
        mytell "the following are the items in the room:"
        describe nL
  case (v, p, moveL positions p c, flip Map.lookup eRM =<< moveL gM p c) of
    (False, _, _,  _) -> mytell . info $ HiddenError
    (True, x, Just y, Nothing) -> successRun y
    (True, x, Just y, Just zs) -> do
      let haveEverything = foldr ((&&) . flip isAvailable g) True zs
      when haveEverything $ successRun y
      unless haveEverything $ do
        mytell . failuer $ c
        mytell . info $ UnavailableAssetsError
    _ -> mytell . failuer $ c
        

-- A runner that makes the valid movements and updates the state.
run :: Command -> GameEnv ()
run = enter positions entryRequirements
