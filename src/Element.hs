module Element where

type ElementID = String

isPickable :: ElementID -> Bool
isPickable = const True

