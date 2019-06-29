module Command.Bag where

import Control.Monad (unless, when)
import Control.Monad.Trans.RWS.Strict
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Element
import Game
import Location

data Command
  = PickUp ElementID
  | Drop ElementID

parse :: [String] -> Maybe Command
parse (x:y:xs)
  | x ++ y == "pickup" = Just . PickUp . toElementID . unwords $ xs
  | x == "drop" = Just . Drop . toElementID . unwords $ (y : xs)
  | otherwise = Nothing
parse _ = Nothing

run :: Command -> GameEnv ()
run (PickUp x) = do
  g <- get
  when (inLocationAndPickable g) $ do
    tellN
      "Picked up the item. You can now use this to make new items or unlock new paths."
    put $ pickItem x g
  unless (inLocationAndPickable g) $
    tellN
      "Unable to pick up the item. It either not pickable or is not in the room."
  where
    inLocationAndPickable (Game p _ _ eM) = eM ! x == H p && isPickable x
run (Drop x) = do
  g <- get
  when (inBag g) $ do
    tellN "Dropped the item. You can pick it from here if you ever need it."
    put $ dropItem x g
    unless (inBag g) $ tell "Ah! You cannot drop something that you do not have"
  where
    inBag (Game _ _ _ eM) = eM ! x == Location.Bag

pickItem x (Game p v m eM) = Game p v m eM'
  where
    eM' = Map.insert x Bag eM

dropItem x (Game p v m eM) = Game p v m eM'
  where
    eM' = Map.insert x (H p) eM
