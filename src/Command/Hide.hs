-- This module defines commands for hiding/unhiding.
module Command.Hide where

import Control.Monad (unless, when)
import Control.Monad.Trans.RWS.Strict
import Game.Internal

-- Basic command data type.
data Command = Hide Element | Unhide

instance ResponseMessage Command where
  success (Hide e) = "successfully hidden. nobody can see you now."
  success Unhide = "successfully unhidden. people can see you now."
  failuer (Hide e) = "can't hide here."
  failuer Unhide = "can't unhide."

-- Parser that parses and outputs the command.
parse :: [String] -> Maybe Command
parse ["unhide"] = Just Unhide
parse ("hide":y:xs) = do
  e <- toAsset . unwords $ xs
  case (y, e) of
    ("under", Table _) -> Just . Hide $ e
    ("inside", Cabinet _) -> Just . Hide $ e
    _ -> Nothing
parse _ = Nothing

-- The runner to execute the functionality.
run :: Command -> GameEnv ()
run c@(Hide e) = do
  g <- get
  when (isAvailable e g) $ do
    put $ g { visibility = False }
    mytell . success $ c
  unless (isAvailable e g) $ mytell . failuer $ c
run c@Unhide = do
  g <- get
  put $ g { visibility = True }
  mytell . success $ c
