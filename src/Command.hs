-- This module contains the compilation of all the commands.
module Command where

import qualified Command.Bag as Bag
import qualified Command.Hide as Hide
import qualified Command.Make as Make
import qualified Command.Movement as Movement
import qualified Command.Switch as Switch
import Game.Internal

-- The combined data type describing all the commands.
data Command
  = CMo Movement.Command
  | CB Bag.Command
  | CH Hide.Command
  | CMa Make.Command
  | CS Switch.Command
  | NOP

lM :: Maybe a -> Maybe a -> Maybe a
lM Nothing x = x
lM x Nothing = x

-- A chained parser made by combining multiple parsers.
-- Checks all the parsers to get the proper result.
parse :: [String] -> Maybe Command
parse x =
  lM (CB <$> Bag.parse x) .
  lM (CMo <$> Movement.parse x) .
  lM (CH <$> Hide.parse x) .
  lM (CMa <$> Make.parse x) . lM (CS <$> Switch.parse x) $
  Nothing

-- A simple runner function.
run :: Command -> GameEnv ()
run (CMo x) = Movement.run x
run (CB x) = Bag.run x
run (CH x) = Hide.run x
run (CMa x) = Make.run x
run (CS x) = Switch.run x
run NOP = return ()
