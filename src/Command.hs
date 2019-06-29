module Command where

import qualified Command.Bag as Bag
import qualified Command.Movement as Movement
import qualified Command.Hide as Hide
import qualified Command.Make as Make
import qualified Command.Switch as Switch
import Game

data Command
  = Mo Movement.Command
  | B Bag.Command
  | H Hide.Command
  | Ma Make.Command
  | S Switch.Command

lM :: Maybe a -> Maybe a -> Maybe a
lM Nothing x = x
lM x Nothing = x

parse :: [String] -> Maybe Command
parse x = lM (B <$> Bag.parse x)
        . lM (Mo <$> Movement.parse x)
        . lM (H <$> Hide.parse x)
        . lM (Ma <$> Make.parse x)
        . lM (S <$> Switch.parse x) $ Nothing

run :: Command -> GameEnv ()
run (Mo x) = Movement.run x
run (B x) = Bag.run x
run (H x) = Hide.run x
run (Ma x) = Make.run x
run (S x) = Switch.run x
