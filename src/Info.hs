module Info where

import Element

class HasInfo a where
  info :: a -> String

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

instance HasInfo Location where
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


