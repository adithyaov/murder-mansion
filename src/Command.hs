module Command where

import qualified Command.Bag as Bag
import qualified Command.Movement as Movement

data Command
  = M Movement.Command
  | B Bag.Command

parse :: [String] -> Maybe Command
parse _ = Nothing
