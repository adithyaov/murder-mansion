module Command.Bag where

import Control.Monad (unless, when)
import Control.Monad.Trans.RWS.Strict
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Element
import Game
import Location

data Command
  = PickUp Element
  | Drop Element

parse :: [String] -> Maybe Command
parse (x:y:xs)
  | x ++ y == "pickup" = Just . PickUp . toElement . unwords $ xs
  | x == "drop" = Just . Drop . toElement . unwords $ (y : xs)
  | otherwise = Nothing
parse _ = Nothing

run :: Command -> GameEnv ()
run (PickUp x) = do
  g@(Game p _ _ eM) <- get
  let inLocationAndPickable = eM ! x == H p && isPickable x
  when inLocationAndPickable $ do
    tellN
      "Picked up the item. You can now use this to make new items or unlock new paths."
    put $ pickItem x g
  unless inLocationAndPickable $
    tellN
      "Unable to pick up the item. It either not pickable or is not in the room."
run (Drop x) = do
  g@(Game _ _ _ eM) <- get
  let inBag = eM ! x == Location.Bag
  when inBag $ do
    tellN "Dropped the item. You can pick it from here if you ever need it."
    put $ dropItem x g
    unless inBag $ tell "Ah! You cannot drop something that you do not have"

pickItem x (Game p v m eM) = Game p v m eM'
  where
    eM' = Map.insert x Bag eM

dropItem x (Game p v m eM) = Game p v m eM'
  where
    eM' = Map.insert x (H p) eM
