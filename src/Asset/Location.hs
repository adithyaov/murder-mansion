module Asset.Location where

import Asset.Internal

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


instance HasStringID House where
  fromAsset LivingArea = "living area"
  fromAsset Pool = "pool"
  fromAsset TreeHouseGroundFloor = "tree house ground floor"
  fromAsset TreeHouseFirstFloor = "tree house first floor"
  fromAsset FireRoom = "fire room"
  fromAsset Garage = "garage"
  fromAsset HallWayFirstFloor = "hallway first floor"
  fromAsset MasterBedroom = "master bedroom"
  fromAsset BathroomMasterBedroom = "master bedroom bathroom"
  fromAsset DiningHall = "dining hall"
  fromAsset PlayRoom = "play room"
  fromAsset BathroomPlayRoom = "play room bathroom"
  fromAsset SmokingArea = "smoking area"
  fromAsset HomeTheater = "home theater"
  fromAsset HallWaySecondFloor = "hallway second floor"
  fromAsset HallWayThirdFloor = "hallway third floor"
  fromAsset StorageRoom = "storage room"
  fromAsset BalconyThirdFloor = "balcony on the third floor"
  fromAsset Kitchen = "kitchen"
  fromAsset KitchenStorage = "kitchen storage"
  fromAsset HallwayFourthFloor = "hallway fourth floor"
  fromAsset GeneratorRoom = "generator room"
  fromAsset ChemistryLab = "chemistry lab"
  fromAsset Exit = "exit"

  toAsset "living area" = Just LivingArea 
  toAsset "pool" = Just Pool 
  toAsset "tree house ground floor" = Just TreeHouseGroundFloor 
  toAsset "tree house first floor" = Just TreeHouseFirstFloor 
  toAsset "fire room" = Just FireRoom 
  toAsset "garage" = Just Garage 
  toAsset "hallway first floor" = Just HallWayFirstFloor 
  toAsset "master bedroom" = Just MasterBedroom 
  toAsset "master bedroom bathroom" = Just BathroomMasterBedroom 
  toAsset "dining hall" = Just DiningHall 
  toAsset "play room" = Just PlayRoom 
  toAsset "play room bathroom" = Just BathroomPlayRoom 
  toAsset "smoking area" = Just SmokingArea 
  toAsset "home theater" = Just HomeTheater 
  toAsset "hallway second floor" = Just HallWaySecondFloor 
  toAsset "hallway third floor" = Just HallWayThirdFloor 
  toAsset "storage room" = Just StorageRoom 
  toAsset "balcony on the third floor" = Just BalconyThirdFloor 
  toAsset "kitchen" = Just Kitchen 
  toAsset "kitchen storage" = Just KitchenStorage 
  toAsset "hallway fourth floor" = Just HallwayFourthFloor 
  toAsset "generator room" = Just GeneratorRoom 
  toAsset "chemistry lab" = Just ChemistryLab 
  toAsset "exit" = Just Exit 
  toAsset _ = Nothing

instance HasStringID Location where
  fromAsset Bag = "bag"
  fromAsset (H x) = fromAsset x 
  fromAsset None = "none"

  toAsset "bag" = Just Bag
  toAsset "none" = Just None
  toAsset x = H <$> toAsset x

instance HasInfo House where
  info LivingArea = "this is the living area, the room where it all started."
  info Pool = "the pool.....dead bodies everywhere..."
  info TreeHouseGroundFloor = "the ground floor of the tree house looks clean. wait, that's a ladder."
  info TreeHouseFirstFloor = "the tree house first floor. lets see what we find here."
  info FireRoom = "the fire room has a furnace. one should probably inspect the furnace."
  info Garage = "the garage seems rather empty for such a huge house. where are all the cars???"
  info HallWayFirstFloor = "the hallway of the first floor. i need to explore this house asap."
  info MasterBedroom = "the master bedroom is huge!"
  info BathroomMasterBedroom = "the bathroom of master bedroom is slippery....omg that's blood."
  info DiningHall = "the dining hall, it's rather disgusting. all the food is stale and it has an unbearable stench."
  info PlayRoom = "a play room. i hope the kids are safe somewhere..."
  info BathroomPlayRoom = "the play room bathroom has a wierd smell....oh no, the kids...."
  info SmokingArea = "a smoking area....but no ash tray."
  info HomeTheater = "the home theater is huge and the screen just shows static. my eyes hurt."
  info HallWaySecondFloor = "the hallway of the second floor. i need to explore this house asap."
  info HallWayThirdFloor = "the hallway of the third floor. i need to explore this house asap."
  info StorageRoom = "the storage room is a little dark, i hope i find something important here."
  info BalconyThirdFloor = "a balcony on the third floor. the smell of fresh air is beautiful."
  info Kitchen = "the kitchen is covered in blood and the knives are missing from their shelves."
  info KitchenStorage = "the kitchen storage is rather small."
  info HallwayFourthFloor = "the hallway of the fourth floor. i need to explore this house asap."
  info GeneratorRoom = "the generator room, i see a generator here."
  info ChemistryLab = "the chemistry lab has all kinds of chemicals. sulphuric acid is missing though. the murderer must have used it."
  info Exit = "the exit. yes, finally, freeeeeedoooooom!"


instance HasInfo Location where
  info Bag = "a bag where you can carry pickable elements."
  info (H x) = info x 
  info None = "this does not exist. it has been consumed to make something else."
