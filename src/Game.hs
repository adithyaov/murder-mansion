module Game where

import Command
import Control.Monad.Trans.RWS.Strict
import Game.Internal
import Asset
import Player
import Murderer
import Control.Monad (when)

data GameStatus = Escaped | Dead | Continue deriving (Eq)

instance HasInfo GameStatus where
  info Escaped = "game over. you have successfully escaped."
  info Dead = "game over. you are dead."
  info Continue = "continue..."

gameSetup :: GameEnv ()
gameSetup = do
  (Game p _ _ _ _) <- get
  mytell . info $ GameStart 
  mytell $ "you're currently in " ++ fromAsset p 
  mytell . info $ p

gameEnd :: GameEnv GameStatus
gameEnd = do
  (Game p _ m _ _) <- get
  endF p m
  where
    endF x y
      | x == y = do
          mytell . info $ Dead
          return Dead
      | x == Exit = do
          mytell . info $ Escaped
          return Escaped
      | otherwise = return Continue

runComp :: Game -> GameEnv a -> IO (a, Game)
runComp g c = do
  (a, s, w) <- runRWST c () g
  putStrLn w
  return (a, s)

runWhenContinue :: GameEnv () -> GameEnv ()
runWhenContinue c = do
  g <- gameEnd
  when (g == Continue) c

game :: IO ()
game = do
  _ <- runComp initialGame gameSetup
  gameLoop initialGame

gameLoop :: Game -> IO ()
gameLoop s0 = do
  (_, s1) <- runComp s0 . runWhenContinue $ playerTurn 
  (_, s2) <- runComp s1 . runWhenContinue $ playerTurn 
  (_, s3) <- runComp s2 . runWhenContinue $ murdererTurn
  (g, _) <- runComp s3 gameEnd
  when (g == Continue) $ gameLoop s3



