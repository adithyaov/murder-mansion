module Test where

import Control.Monad.Trans.RWS.Strict
import Command

mkState s = put s >> get

cmd = run . fromMaybe NOP . parse . words

eqState s = (== s) <$> get
