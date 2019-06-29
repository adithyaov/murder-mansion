module Element where

type ElementID = String

data Element = Element
  { id :: ElementID
  , element :: e
  }

isPickable :: ElementID -> Bool
isPickable = const True

