module Player where

import System.IO
import Command
import Control.Monad.Trans.RWS.Strict
import Game.Internal
import Control.Monad.IO.Class (liftIO)

playerTurn :: GameEnv ()
playerTurn = do
  liftIO . putStr $ commandPrepend
  c <- liftIO $ hFlush stdout >> getLine
  case parse (words c) of
    Nothing -> mytell . info $ ParseError
    Just x -> run x

