module Test.Command.Bag where

import Data.Map (insert)
import Game.Internal
import Command.Bag
import Test

test = do
  s <- mkState $ initialGame { player = Garage }
  cmd "pick up clay" 
  e1 <- eqState $ s { elementMap = insert Clay Bag elementMap }
  mkState $ s { player = Pool }
  cmd "drop clay"
  e2 <- eqState $ s { elementMap = insert Clay (H Pool) elementMap }
  return [e1, e2]

