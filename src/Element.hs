module Element where

data Color = Red | Blue | Green deriving (Ord, Show, Eq)

data Element = StorageKey | ExitKey | Table Color | Cabinet Color deriving (Eq, Ord)

storageKey = StorageKey
exitKey = ExitKey

toElement :: String -> Element
toElement "storage key" = StorageKey
toElement "exit key" = ExitKey

isPickable :: Element -> Bool
isPickable = const True

