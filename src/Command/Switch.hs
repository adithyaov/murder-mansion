module Command.Switch where

import Control.Monad.Trans.RWS.Strict
import Game

data Command = TurnOn Element | TurnOff Element

parse :: [String] -> Maybe Command
parse ("turn":y:xs) = do
  e <- toAsset . unwords $ xs
  case (y, e) of
    ("on", Generator) -> Just . TurnOn $ e
    ("off", Generator) -> Just . TurnOff $ e
    _ -> Nothing
parse _ = Nothing

run :: Command -> GameEnv ()
run (TurnOn Generator) = do
  g <- get
  put $ g { electricity = True }
  tellN "The electricity is now swictched on."
run (TurnOff Generator) = do
  g <- get
  put $ g { electricity = False }
  tellN "The electricity is now swictched off."
run _ = tell "Nope, Can't turn that on."
