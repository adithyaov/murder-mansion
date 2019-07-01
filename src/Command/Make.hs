module Command.Make where

import Control.Monad (unless, when)
import Control.Monad.Trans.RWS.Strict
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Game

newtype Command = Make Element

parse :: [String] -> Maybe Command
parse ("make":xs) = fmap Make $ toAsset . unwords $ xs
parse _ = Nothing

run :: Command -> GameEnv ()
run (Make ExitKey) = do
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
    tellN
      "Finally, Made the exit key. I need to get out of this place ASAP!"
    put $ g { elementMap = nEM }
  unless haveEverything $
    tellN
      "Can't make the lock, few ingriedients are missing."
run (Make Mold) = do
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
    tellN
      "Made the mold. A step closer to freedom."
    put $ g { elementMap = nEM }
  unless haveEverything $
    tellN
      "Can't make mold, few ingriedients are missing."
run _ = tell "Nope, Can't make that in this life."
