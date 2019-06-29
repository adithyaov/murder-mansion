module Location where

data Location
  = H House
  | Bag
  | None
  deriving (Eq, Ord, Show)

-- The following are the locations in the house.
data House
  = LivingArea
  | Pool
  | TreeHouseGroundFloor
  | TreeHouseFirstFloor
  | FireRoom
  | Garage
  | HallWayFirstFloor
  | MasterBedroom
  | BathroomMasterBedroom
  | DiningHall
  | PlayRoom
  | BathroomPlayRoom
  | SmokingArea
  | HomeTheater
  | HallWaySecondFloor
  | HallWayThirdFloor
  | StorageRoom
  | BalconyThirdFloor
  | Kitchen
  | KitchenStorage
  | HallwayFourthFloor
  | GeneratorRoom
  | ChemistryLab
  | Exit
  deriving (Eq, Ord, Show)
