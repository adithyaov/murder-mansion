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

game :: IO ()
game = do
  _ <- runComp initialGame gameSetup
  gameLoop initialGame

gameLoop :: Game -> IO ()
gameLoop s0 = do
  (_, s1) <- runComp s0 playerTurn 
  (g1, _) <- runComp s1 gameEnd
  when (g1 == Continue) $ do
    (_, s2) <- runComp s1 playerTurn 
    (g2, _) <- runComp s2 gameEnd
    when (g2 == Continue) $ do
      (_, s3) <- runComp s2 murdererTurn
      (g3, _) <- runComp s2 gameEnd
      when (g3 == Continue) $ gameLoop s3



