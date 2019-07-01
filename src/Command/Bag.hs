module Command.Bag where

import Control.Monad (unless, when)
import Control.Monad.Trans.RWS.Strict
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Game

data Command
  = PickUp Element
  | Drop Element

instance ResponseMessage Command where
  success (PickUp e) = "picked up " ++ fromAsset e ++ "\n" ++ info e
  success (Drop e) = "dropped " ++ fromAsset e
  failuer (PickUp e) = "unable to pick up " ++ fromAsset e
  failuer (Drop e) = "unable to drop " ++ fromAsset e

parse :: [String] -> Maybe Command
parse (x:y:xs)
  | x ++ y == "pickup" = fmap PickUp $ toAsset . unwords $ xs
  | x == "drop" = fmap Drop $ toAsset . unwords $ (y : xs)
  | otherwise = Nothing
parse _ = Nothing

run :: Command -> GameEnv ()
run c@(PickUp x) = do
  g@(Game p _ _ eM _) <- get
  let inLocationAndPickable = eM ! x == H p && isPickable x
  when inLocationAndPickable $ do
    put $ pickItem x g
    tell . success $ c
  unless inLocationAndPickable $
    tell . failuer $ c
run c@(Drop x) = do
  g@(Game _ _ _ eM _) <- get
  let inBag = eM ! x == Bag
  when inBag $ do
    put $ dropItem x g
    tell . success $ c
    unless inBag $ tell . failuer $ c

pickItem x (Game p v m eM e) = Game p v m eM' e
  where
    eM' = Map.insert x Bag eM

dropItem x (Game p v m eM e) = Game p v m eM' e
  where
    eM' = Map.insert x (H p) eM
