module Command.Hide where

import Control.Monad (unless, when)
import Control.Monad.Trans.RWS.Strict
import Game.Internal

data Command = Hide Element | Unhide

instance ResponseMessage Command where
  success (Hide e) = "successfully hidden. nobody can see you now."
  success Unhide = "successfully unhidden. people can see you now."
  failuer (Hide e) = "can't hide here."
  failuer Unhide = "can't unhide."

parse :: [String] -> Maybe Command
parse ["unhide"] = Just Unhide
parse ("hide":y:xs) = do
  e <- toAsset . unwords $ xs
  case (y, e) of
    ("under", Table _) -> Just . Hide $ e
    ("inside", Cabinet _) -> Just . Hide $ e
    _ -> Nothing
parse _ = Nothing

run :: Command -> GameEnv ()
run c@(Hide _) = do
  g <- get
  put $ g { visibility = False }
  mytell . success $ c
run c@Unhide = do
  g <- get
  put $ g { visibility = True }
  mytell . success $ c
