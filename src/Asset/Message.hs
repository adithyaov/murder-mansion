module Asset.Message where

import Asset.Internal

data Error
  = ParseError
  | InGameError InGameError deriving (Eq, Ord, Show)

data InGameError
  = UnavailableAssetsError
  | UnPickableError
  | HiddenError deriving (Eq, Ord, Show)

instance HasInfo Error where
  info ParseError = "there is a parse error, this command is not valid at this moment."
  info (InGameError e) = info e

instance HasInfo InGameError where
  info UnavailableAssetsError = "some assets that you need to perform this action are unavailable."
  info UnPickableError = "can't pick this item, it's unpickable."
  info HiddenError = "you are hidden, can't move while hidden."
