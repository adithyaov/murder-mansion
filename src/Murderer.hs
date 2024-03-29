-- This is a simple murderer AI that randomly chooses valid paths.
module Murderer where

import Control.Monad.Trans.RWS.Strict
import Game.Internal
import Control.Monad.IO.Class (liftIO)
import Command.Movement
import Data.Maybe (mapMaybe)
import System.Random
import Game.Map

-- Valid move directions of the murderer.
moveDirections :: [Command]
moveDirections = [North, South, East, West, Up, Down]

-- Valid locations the murderer can move given the locations he/she is in.
validMoveLocations :: House -> [Command] -> [House]
validMoveLocations h = mapMaybe (moveL positions h) 

-- A generic function to choose random element from a list.
listRand :: (Show a) => [a] -> IO a
listRand l = do
  i <- randomRIO (0, length l - 1)
  return $ l !! i

-- A simple diff function. Not used anywhere yet.
diff :: (Int, Int, Int) -> (Int, Int, Int) -> Int
diff (x, y, z) (a, b, c) = abs (x - a) + abs (y - b) + abs (z - c)

-- The murderer turn defines the murderer movement.
murdererTurn :: GameEnv ()
murdererTurn = do
  g@(Game p v m _ _) <- get
  mytell "the murderer is on the move."
  nL <- liftIO . listRand . validMoveLocations m $ moveDirections 
  put $ g { murderer = nL }
  mytell $ "my hunch tells me that he is in " ++ fromAsset nL
