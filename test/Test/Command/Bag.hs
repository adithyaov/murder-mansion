module Test.Command.Bag where

import Control.Monad (unless, when)
import Control.Monad.Trans.RWS.Strict
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Game.Internal
import Command.Bag

testRun :: IO ()
testRun = do
  test $ run (PickUp 
  


run :: Command -> GameEnv ()
run c@(PickUp x) = do
  g@(Game p _ _ eM _) <- get
  let inLocationAndPickable = eM ! x == H p && isPickable x
  when inLocationAndPickable $ do
    put $ pickItem x g
    mytell . success $ c
  unless inLocationAndPickable $
    mytell . failuer $ c
run c@(Drop x) = do
  g@(Game _ _ _ eM _) <- get
  let inBag = eM ! x == Bag
  when inBag $ do
    put $ dropItem x g
    mytell . success $ c
    unless inBag $ mytell . failuer $ c

pickItem x (Game p v m eM e) = Game p v m eM' e
  where
    eM' = Map.insert x Bag eM

dropItem x (Game p v m eM e) = Game p v m eM' e
  where
    eM' = Map.insert x (H p) eM
