module Command.Movement where

import Control.Monad (unless, when)
import Control.Monad.Trans.RWS.Strict
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Map ((!))
import Game
import Map (positions)

data Command
  = North
  | South
  | East
  | West
  | Up
  | Down

instance ResponseMessage Command where
  success _ = "successfully moved."
  failuer _ = "failed to move, there is no path here."

parse :: [String] -> Maybe Command
parse ["go", x]
  | x == "north" = Just North
  | x == "south" = Just South
  | x == "east" = Just East
  | x == "west" = Just West
  | x == "up" = Just Up
  | x == "down" = Just Down
  | otherwise = Nothing
parse _ = Nothing

moveC :: Command -> (Int, Int, Int) -> (Int, Int, Int)
moveC North (x, y, z) = (x, y + 1, z)
moveC South (x, y, z) = (x, y - 1, z)
moveC East (x, y, z) = (x + 1, y, z)
moveC West (x, y, z) = (x - 1, y, z)
moveC Up (x, y, z) = (x, y, z + 1)
moveC Down (x, y, z) = (x, y, z - 1)

moveL :: Command -> House -> Maybe House
moveL c x =
  Bimap.lookupR x positions >>= Just . moveC c >>= flip Bimap.lookup positions

run :: Command -> GameEnv ()
run c = do
  (Game p v m eM e) <- get
  let storageCheck = eM ! StorageKey == Bag
      exitCheck = eM ! ExitKey == Bag
      visibilityCheck = v
      successRun nL = do
        put $ Game nL v m eM e
        tell . success $ c
        tell $ "you're currently in " ++ fromAsset nL
        tell . info $ nL
      runStorageRoom nL = do
        when storageCheck $ successRun nL
        unless storageCheck $ tell . info . InGameError $ UnavailableAssetsError
  unless visibilityCheck $
    tell . info . InGameError $ HiddenError 
  case (p, moveL c p) of
    (_, Nothing) -> tell . failuer $ c
    (StorageRoom, Just nL) -> runStorageRoom nL
    (_, Just StorageRoom) -> runStorageRoom StorageRoom
    (_, Just Exit) -> do
      when exitCheck $ successRun Exit
      unless exitCheck $ tell . info . InGameError $ UnavailableAssetsError
    (_, Just nL) -> successRun nL
