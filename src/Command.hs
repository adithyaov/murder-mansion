module Command where

import qualified Command.Movement as Movement
import qualified Command.Bag as Bag

data Command = M Movement.Command | B Bag.Command

parse :: [String] -> Maybe Command
parse _ = Nothing
