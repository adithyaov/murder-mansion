-- This module defines the working of a switch in the game. Boolean operations.
module Command.Switch where

import Control.Monad.Trans.RWS.Strict
import Game.Internal

-- Basic data type for command.
data Command = TurnOn Element | TurnOff Element

-- A simple parser.
parse :: [String] -> Maybe Command
parse ("turn":y:xs) = do
  e <- toAsset . unwords $ xs
  case (y, e) of
    ("on", Generator) -> Just . TurnOn $ e
    ("off", Generator) -> Just . TurnOff $ e
    _ -> Nothing
parse _ = Nothing

-- The runner which current only supports the switch for the generator.
run :: Command -> GameEnv ()
run (TurnOn Generator) = do
  g <- get
  put $ g { electricity = True }
  mytell "The electricity is now swictched on."
run (TurnOff Generator) = do
  g <- get
  put $ g { electricity = False }
  mytell "The electricity is now swictched off."
run _ = mytell "Nope, Can't turn that on."
