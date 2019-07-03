module Asset.Element where

import Asset.Internal

data Color
  = Red
  | Blue
  | Green
  deriving (Ord, Eq, Show)

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

instance HasStringID Element where
  fromAsset StorageKey = "storage key"
  fromAsset ExitKey = "exit key"
  fromAsset (Table Red) = "red table"
  fromAsset (Table Green) = "green table"
  fromAsset (Table Blue) = "blue table"
  fromAsset (Cabinet Red) = "red cabinet"
  fromAsset (Cabinet Green) = "green cabinet"
  fromAsset (Cabinet Blue) = "blue cabinet"
  fromAsset Generator = "generator"
  fromAsset Furnace = "furnace"
  fromAsset Oil = "oil"
  fromAsset Lighter = "lighter"
  fromAsset Steel = "steel"
  fromAsset GasZ = "gas z"
  fromAsset Clay = "clay"
  fromAsset ChemicalChamber = "chemical chamber"
  fromAsset Mold = "mold"

  toAsset "storage key" = Just StorageKey
  toAsset "exit key" = Just ExitKey
  toAsset "red table" = Just (Table Red)
  toAsset "green table" = Just (Table Green)
  toAsset "blue table" = Just (Table Blue)
  toAsset "red cabinet" = Just (Cabinet Red)
  toAsset "green cabinet" = Just (Cabinet Green)
  toAsset "blue cabinet" = Just (Cabinet Blue)
  toAsset "generator" = Just Generator
  toAsset "furnace" = Just Furnace
  toAsset "oil" = Just Oil
  toAsset "lighter" = Just Lighter
  toAsset "steel" = Just Steel
  toAsset "gas z" = Just GasZ
  toAsset "clay" = Just Clay
  toAsset "chemical chamber" = Just ChemicalChamber
  toAsset "mold" = Just Mold
  toAsset _ = Nothing

instance IsPickable Element where
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

instance HasInfo Element where
  info StorageKey = "this key lets you enter the storage room. The storage room might give some hints about escaping"
  info ExitKey = "this key unlocks the exit door. this is the most important item in this house."
  info (Table x) = "this is a simple table, dont let the color fool you. One can probably hide under it."
  info (Cabinet x) = "a simple cabinet. it's empty so one can probably hide inside that."
  info Generator = "the generator can be used to turn on the electricity in this mansion."
  info Furnace = "the furnace can be used to make the exit key if you have the required ingridients, namely, mold and steel. you will need a lighter and oil in the vicinity to use it though."
  info Oil = "oil is required in the vicinity to use the furnace."
  info Lighter = "a lighter is required in the vicinity to use the furnace"
  info Steel = "steel is a required item to make the exit key."
  info GasZ = "gas z along with clay can be used to make the mold if you have a chemical chamber in the vicinity."
  info Clay = "clay along with gas z can be used to make the mold if you have a chemical chamber in the vicinity."
  info ChemicalChamber = "a chemical chamber can perform chemical reactions, it can be used to make mold if you have clay and gas z in the vicinity."
  info Mold = "mold is required to make the main key. you will have to use it near a furnace with steel in the vicinity"
