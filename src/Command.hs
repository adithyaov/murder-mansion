module Command where

import Command.Movement as Movement

data Command = Go Direction | PickUp String | Drop String

parse :: [String] -> Maybe Command
parse [x, y]
  | x == "go" = Go <$> Movement.parse y
  | otherwise = Nothing
parse (x:y:xs)
  | x ++ y == "pickup" = Just . PickUp . unwords $ xs 
  | x == "drop" = Just . Drop . unwords $ (y:xs)
  | otherwise = Nothing
parse _ = Nothing
