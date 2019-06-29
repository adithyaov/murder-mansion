module Game where

import BP
import Data.Map
import Element.Internal
import Data.Set

data Game = Interactable e => Game
  { player :: Location
  , murderer :: Location
  , elementMap :: Map ElementID Location
  }

