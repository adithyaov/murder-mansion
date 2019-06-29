module Command.Bag where

import Game
import Location
import Element
import Data.Map (Map, (!))
import qualified Data.Map as Map

data Command = PickUp String | Drop String

parse :: [String] -> Maybe Command
parse (x:y:xs)
  | x ++ y == "pickup" = Just . PickUp . unwords $ xs 
  | x == "drop" = Just . Drop . unwords $ (y:xs)
  | otherwise = Nothing
parse _ = Nothing

run :: Command -> Game -> Maybe Game
run (PickUp x) g@(Game p _ eM)
  | eM ! x == p && isPickable x = Just . pickItem x $ g
  | otherwise = Nothing
run (Drop x) g@(Game p _ eM)
  | eM ! x == Bag = Just . dropItem x $ g
  | otherwise = Nothing

pickItem x (Game p m eM) = Game p m eM'
  where
    eM' = Map.insert x Bag eM

dropItem x (Game p m eM) = Game p m eM'
  where
    eM' = Map.insert x p eM


