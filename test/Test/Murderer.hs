module Murderer where

import Control.Monad.Trans.RWS.Strict
import Game.Internal
import Control.Monad.IO.Class (liftIO)
import Command.Movement
import Data.Maybe (mapMaybe)
import System.Random

moveDirections :: [Command]
moveDirections = [North, South, East, West, Up, Down]

validMoveLocations :: House -> [Command] -> [House]
validMoveLocations h = mapMaybe (`moveL` h) 

listRand :: (Show a) => [a] -> IO a
listRand l = do
  i <- randomRIO (0, length l - 1)
  return $ l !! i

diff :: (Int, Int, Int) -> (Int, Int, Int) -> Int
diff (x, y, z) (a, b, c) = abs (x - a) + abs (y - b) + abs (z - c)

murdererTurn :: GameEnv ()
murdererTurn = do
  g@(Game p v m _ _) <- get
  mytell "the murderer is on the move."
  nL <- liftIO . listRand . validMoveLocations m $ moveDirections 
  put $ g { murderer = nL }
  mytell $ "my hunch tells me that he is in " ++ fromAsset nL
