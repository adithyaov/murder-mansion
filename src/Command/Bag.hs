module Command.Bag where

import Game
import Location
import Element

data Command = PickUp String | Drop String

parse :: [String] -> Just Direction
parse (x:y:xs)
  | x ++ y == "pickup" = Just . PickUp . unwords $ xs 
  | x == "drop" = Just . Drop . unwords $ (y:xs)
  | otherwise = Nothing
parse _ = Nothing

run :: Command -> Game -> Just Game
run (PickUp x) g@(Game p _ eM)
  | eM ! x == p && isPickable x = Just $ pickup x g
  | otherwise = Nothing
run (Drop x) g@(Game p _ eM)
  | eM ! x == Bag = Just $ drop x
  | otherwise = Nothing

pickup x (Game p m eM) = Game p m eM'
  where
    eM' = Map.insert x Bag eM

drop x (Game p m e) = Game p m eM'
  where
    eM' = Map.insert x p eM


