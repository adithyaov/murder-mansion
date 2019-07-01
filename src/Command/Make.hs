module Command.Make where

import Control.Monad (unless, when)
import Control.Monad.Trans.RWS.Strict
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Game

newtype Command = Make Element

instance ResponseMessage Command where
  success (Make e) = "successfully made " ++ fromAsset e ++ "\n" ++ info e
  failuer (Make e) = "couldn't make " ++ fromAsset e

parse :: [String] -> Maybe Command
parse ("make":xs) = fmap Make $ toAsset . unwords $ xs
parse _ = Nothing

run :: Command -> GameEnv ()
run c@(Make ExitKey) = do
  g@(Game _ _ _ eM _) <- get
  let moldAndSteelAvailable = isAvailable Steel g && isAvailable Mold g
      furnaceAvailable = isAvailable Furnace g
      oilAndLighterAvailable = isAvailable Oil g && isAvailable Lighter g
      haveEverything = moldAndSteelAvailable && furnaceAvailable && oilAndLighterAvailable
      nEM = Map.insert Mold None
          . Map.insert Steel None
          . Map.insert Oil None
          . Map.insert ExitKey Bag
          $ eM
  when haveEverything $ do
    put $ g { elementMap = nEM }
    tell . success $ c
  unless haveEverything $ tell . failuer $ c
run c@(Make Mold) = do
  g@(Game _ _ _ eM _) <- get
  let clayAvailable = isAvailable Clay g
      gasZAvailable = isAvailable GasZ g
      chemChamberAvailable = isAvailable ChemicalChamber g
      haveEverything = clayAvailable && gasZAvailable && chemChamberAvailable
      nEM = Map.insert Mold Bag
          . Map.insert Clay None
          . Map.insert GasZ None
          $ eM
  when haveEverything $ do
    put $ g { elementMap = nEM }
    tell . success $ c
  unless haveEverything $ tell . failuer $ c
run c = tell . failuer $ c
