-- This the main game module.
module Game where

import Command
import Control.Monad.Trans.RWS.Strict
import Game.Internal
import Asset
import Player
import Murderer
import Control.Monad (when, unless)

-- A default game status that affects the workflow.
data GameStatus = Escaped | Dead | Continue deriving (Eq)

instance HasInfo GameStatus where
  info Escaped = "game over. you have successfully escaped."
  info Dead = "game over. you are dead."
  info Continue = "continue..."

-- The initial game setup. Things you do before the game starts.
gameSetup :: GameEnv ()
gameSetup = do
  (Game p _ _ _ _) <- get
  mytell . info $ GameStart 
  mytell $ "you're currently in " ++ fromAsset p 
  mytell . info $ p

-- This functions gives game status.
gameStatus :: GameEnv GameStatus
gameStatus = do
  (Game p _ m _ _) <- get
  endF p m
  where
    endF x y
      | x == y = return Dead
      | x == Exit = return Escaped
      | otherwise = return Continue

-- A helper to run a specific computation.
runComp :: Game -> GameEnv a -> IO (a, Game)
runComp g c = do
  (a, s, w) <- runRWST c () g
  putStrLn w
  return (a, s)

-- A helper to run case computations.
runWhenContinue :: GameEnv () -> GameEnv ()
runWhenContinue c = do
  g <- gameStatus
  when (g == Continue) c

-- The main game contating the game loop.
game :: IO ()
game = do
  _ <- runComp initialGame gameSetup
  gameLoop initialGame

-- The game loop.
gameLoop :: Game -> IO ()
gameLoop s0 = do
  (_, s1) <- runComp s0 . runWhenContinue $ playerTurn 
  (_, s2) <- runComp s1 . runWhenContinue $ playerTurn 
  (_, s3) <- runComp s2 . runWhenContinue $ murdererTurn
  (g, _) <- runComp s3 gameStatus
  when (g == Continue) $ gameLoop s3
  unless (g == Continue) $ putStrLn . info $ g



