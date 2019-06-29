module Element where

type ElementID = String

storageKey = "storage key"
exitKey = "exit key"

toElementID :: String -> ElementID
toElementID x = x

isPickable :: ElementID -> Bool
isPickable = const True

