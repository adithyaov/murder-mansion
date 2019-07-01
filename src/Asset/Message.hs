module Asset.Message where

import Asset.Internal

data Help
  = GameStart

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

instance HasInfo Help where
  info GameStart = "this is the begning of your journey. there is a murderer on the loose in this house and the house is locked. you need to somehow exit the house through the exit door. explore the house and find a way to escape."
