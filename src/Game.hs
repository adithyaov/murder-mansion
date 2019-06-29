module Game where

import Location
import Data.Map (Map)
import qualified Data.Map as Map
import Element
import Control.Monad.Trans.RWS.Strict

data Game = Game
  { player :: House
  , visibility :: Bool
  , murderer :: House
  , elementMap :: Map ElementID Location
  }

type GameEnv = RWS () String Game

tellN w = tell "\n" >> tell w
