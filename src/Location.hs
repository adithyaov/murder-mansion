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
  deriving (Eq, Ord)


instance Show Location where
  show LivingArea = "living area"
  show Pool = "pool"
  show TreeHouseGroundFloor = "tree house ground floor"
  show TreeHouseFirstFloor = "tree house first floor"
  show FireRoom = "fire room"
  show Garage = "garage"
  show HallWayFirstFloor = "hallway first floor"
  show MasterBedroom = "master bedroom"
  show BathroomMasterBedroom = "master bedroom bathroom"
  show DiningHall = "dining hall"
  show PlayRoom = "play room"
  show BathroomPlayRoom = "play room bathroom"
  show SmokingArea = "smoking area"
  show HomeTheater = "home theater"
  show HallWaySecondFloor = "hallway second floor"
  show HallWayThirdFloor = "hallway third floor"
  show StorageRoom = "storage room"
  show BalconyThirdFloor = "balcony on the third floor"
  show Kitchen = "kitchen"
  show KitchenStorage = "kitchen storage"
  show HallwayFourthFloor = "hallway fourth floor"
  show GeneratorRoom = "generator room"
  show ChemistryLab = "chemistry lab"
  show Exit = "exit"


