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
  deriving (Eq, Ord)

storageKey = StorageKey

exitKey = ExitKey

toElement :: String -> Element
toElement "storage key" = StorageKey
toElement "exit key" = ExitKey
toElement "red table" = Table Red
toElement "green table" = Table Green
toElement "blue table" = Table Blue
toElement "red cabinet" = Cabinet Red
toElement "green cabinet" = Cabinet Green
toElement "blue cabinet" = Cabinet Blue
toElement "generator" = Generator
toElement "furnace" = Furnace
toElement "oil" = Oil
toElement "lighter" = Lighter
toElement "steel" = Steel
toElement "gas z" = GasZ
toElement "clay" = Clay
toElement "chemical chamber" = ChemicalChamber
toElement _ = error "Invalid element"

isPickable :: Element -> Bool
isPickable = const True
