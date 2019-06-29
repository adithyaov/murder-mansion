module Game where

import Location
import Data.Map (Map)
import qualified Data.Map as Map
import Element

data Game = Game
  { player :: House
  , visibility :: Bool
  , murderer :: House
  , elementMap :: Map ElementID Location
  }

