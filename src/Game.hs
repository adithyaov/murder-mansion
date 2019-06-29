module Game where

import Location
import Data.Map
import Element
import Data.Set

data Game = Game
  { player :: Location
  , murderer :: Location
  , elementMap :: Map ElementID Location
  }

