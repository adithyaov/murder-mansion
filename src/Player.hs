-- This module defines the workflow of a player.
module Player where

import System.IO
import Command
import Control.Monad.Trans.RWS.Strict
import Game.Internal
import Control.Monad.IO.Class (liftIO)

-- Describes what a player can do in their turn.
-- Parse input and change the game state accordingly.
playerTurn :: GameEnv ()
playerTurn = do
  liftIO . putStr $ commandPrepend
  c <- liftIO $ hFlush stdout >> getLine
  case parse (words c) of
    Nothing -> mytell . info $ ParseError
    Just x -> run x

