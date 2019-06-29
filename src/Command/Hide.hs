module Command.Hide where

import Control.Monad (unless, when)
import Control.Monad.Trans.RWS.Strict
import Element
import Game
import Location

data Command = Hide Element | Unhide

parse :: [String] -> Maybe Command
parse [x] =
  case x of
    "unhide" -> Just Unhide
    _ -> Nothing
parse (x:y:xs)
  | x == "hide" = do
      e <- toElement . unwords $ xs
      case (y, e) of
        ("under", Table _) -> Just . Hide $ e
        ("inside", Cabinet _) -> Just . Hide $ e
        _ -> Nothing
  | otherwise = Nothing
parse _ = Nothing

run :: Command -> GameEnv ()
run (Hide (Table _)) = do
  g <- get
  put $ g { visibility = False }
  tellN "Hid under the table. Nobody can see you now."
run (Hide (Cabinet _)) = do
  g <- get
  put $ g { visibility = False }
  tellN "Hid inside the cabinet. Nobody can see you now."
run Unhide = do
  g <- get
  put $ g { visibility = True }
  tellN "Not hiding anymore! Need to escape fast!"
run _ = tell "Nope, Can't hide there."
