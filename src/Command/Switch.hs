-- This module defines the working of a switch in the game. Boolean operations.
module Command.Switch where

import Control.Monad.Trans.RWS.Strict
import Game.Internal
import Control.Monad (when, unless)

-- Basic data type for command.
data Command = TurnOn Element | TurnOff Element

instance ResponseMessage Command where
  success (TurnOn e) = "successfully turned on " ++ fromAsset e
  success (TurnOff e) = "successfully turned off " ++ fromAsset e
  failuer (TurnOn e) = "couldn't turn on " ++ fromAsset e
  failuer (TurnOff e) = "couldn't turn off " ++ fromAsset e

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
run c@(TurnOn Generator) = do
  g <- get
  when (isAvailable Generator g) $ do
    put $ g { electricity = True }
    mytell . success $ c
  unless (isAvailable Generator g) $ mytell . failuer $ c
run c@(TurnOff Generator) = do
  g <- get
  when (isAvailable Generator g) $ do
    put $ g { electricity = False }
    mytell . success $ c
  unless (isAvailable Generator g) $ mytell . failuer $ c
run c = mytell . failuer $ c
