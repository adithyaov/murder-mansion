-- This module contains commands to make new items.
module Command.Make where

import Control.Monad (unless, when)
import Control.Monad.Trans.RWS.Strict
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Game.Internal

-- Basic command data type.
newtype Command = Make Element

instance ResponseMessage Command where
  success (Make e) = "successfully made " ++ fromAsset e ++ "\n" ++ info e
  failuer (Make e) = "couldn't make " ++ fromAsset e

-- Simple parser.
parse :: [String] -> Maybe Command
parse ("make":xs) = fmap Make $ toAsset . unwords $ xs
parse _ = Nothing

-- A function that checks if the conditions ate met for making and then makes the described item.
make :: RecipeMap -> Command -> GameEnv ()
make r c@(Make e) =
  case Map.lookup e r of
    Nothing -> mytell . failuer $ c
    Just xs -> do
      g <- get
      let haveEverything = foldr ((&&) . flip isAvailable g) True . allElements $ xs
      when haveEverything $ do
        let setNoneF = foldr ((.) . \k -> Map.insert k None) id . leftElements $ xs
            nEM = Map.insert e Bag . setNoneF . elementMap $ g
        put $ g { elementMap = nEM }
        mytell . success $ c
      unless haveEverything $ do
        mytell . failuer $ c
        mytell . info $ UnavailableAssetsError

-- A simple runner
run :: Command -> GameEnv ()
run c@(Make Mold) = do
  (Game _ _ _ _ eL) <- get
  when eL $ make recipies c
  unless eL $ do
    mytell . failuer $ c
    mytell . info $ NoElecticity
run x = make recipies x
