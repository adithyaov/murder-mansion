module Player where

import Command
import Control.Monad.Trans.RWS.Strict
import Game.Internal

playerTurn :: Game -> IO Game
playerTurn s = do
  c <- getLine
  case parse (words c) of
    Nothing -> do
      putStrLn . info $ ParseError
      return s
    Just x -> do
      (sN, w) <- execRWST (run x) () s
      putStrLn w
      return sN

loopPlayerTurn :: Game -> IO Game
loopPlayerTurn s = do
  sN <- playerTurn s
  loopPlayerTurn sN
