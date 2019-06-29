module Element where

data Color
  = Red
  | Blue
  | Green
  deriving (Ord, Show, Eq)

data Element
  = StorageKey
  | ExitKey
  | Table Color
  | Cabinet Color
  | Generator
  | Furnace
  | Oil
  | Lighter
  | Steel
  | GasZ
  | Clay
  | ChemicalChamber
  | Mold
  deriving (Eq, Ord, Show)

storageKey = StorageKey

exitKey = ExitKey

toElement :: String -> Maybe Element
toElement "storage key" = Just StorageKey
toElement "exit key" = Just ExitKey
toElement "red table" = Just (Table Red)
toElement "green table" = Just (Table Green)
toElement "blue table" = Just (Table Blue)
toElement "red cabinet" = Just (Cabinet Red)
toElement "green cabinet" = Just (Cabinet Green)
toElement "blue cabinet" = Just (Cabinet Blue)
toElement "generator" = Just Generator
toElement "furnace" = Just Furnace
toElement "oil" = Just Oil
toElement "lighter" = Just Lighter
toElement "steel" = Just Steel
toElement "gas z" = Just GasZ
toElement "clay" = Just Clay
toElement "chemical chamber" = Just ChemicalChamber
toElement "mold" = Just Mold
toElement _ = Nothing

isPickable :: Element -> Bool
isPickable StorageKey = True
isPickable ExitKey = True
isPickable (Table _) = False
isPickable (Cabinet _) = False
isPickable Generator = False
isPickable Furnace = False
isPickable Oil = True
isPickable Lighter = True
isPickable Steel = True
isPickable GasZ = True
isPickable Clay = True
isPickable ChemicalChamber = False
isPickable Mold = True
