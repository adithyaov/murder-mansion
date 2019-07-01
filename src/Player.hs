module Player where

import Game
import Command
import Control.Monad.Trans.RWS.Strict

playerTurn :: Game -> IO Game
playerTurn s = do
  c <- getLine
  case parse (words c) of
    Nothing -> do
      putStrLn $ c ++ " is not a valid command"
      return s
    Just x -> do
      (sN, w) <- execRWST (run x) () s
      putStrLn w
      return sN
      
loopPlayerTurn :: Game -> IO Game
loopPlayerTurn s = do
  sN <- playerTurn s
  print sN
  loopPlayerTurn sN
  
