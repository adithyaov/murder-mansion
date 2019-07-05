import Control.Monad.Trans.RWS.Strict
import Control.Monad
import Command
import Game.Internal
import Data.Maybe (fromMaybe)
import Data.Map ((!))
import qualified Data.Map as Map
import Control.Monad.IO.Class (liftIO)
import Game

mkState s = put s >> get

cmd = run . fromMaybe NOP . parse . words

cmdF x = do
  s1 <- get
  cmd x
  s2 <- get
  return $ s1 == s2

eqState s = (== s) <$> get

goTo l = do
  s <- get
  put $ s { player = l }

putIn e l = do
  s <- get
  put $ s { elementMap = Map.insert e l $ elementMap s }

acc :: String -> Bool -> GameEnv ()
acc s True = liftIO . putStrLn $ "OK: " ++ s
acc s False = do
  liftIO . putStrLn $ "FAIL: " ++ s
  error $ "FAIL: " ++ s

testRunBag :: GameEnv ()
testRunBag = do
  put initialGame
  goTo Garage
  cmd "pick up clay"
  eM <- elementMap <$> get
  acc "pick up succeed" $ (eM ! Clay) == Bag

  goTo Pool
  cmd "drop clay"
  eM <- elementMap <$> get
  acc "drop success" $ eM ! Clay == H Pool
  
  put initialGame
  goTo Garage
  acc "pick up fail" =<< cmdF "pick up gas z"

  put initialGame
  acc "drop fail" =<< cmdF "drop gas z"

  return ()

testRunHide = do
  put initialGame
  goTo MasterBedroom
  cmd "hide under blue table"
  v <- visibility <$> get
  acc "hide success" $ v == False

  cmd "unhide"
  v <- visibility <$> get
  acc "unhide success" $ v == True

  put initialGame
  acc "hide fail" =<< cmdF "hide under blue table"

testRunMake = do
  put initialGame
  Mold `putIn` Bag
  Steel `putIn` Bag
  Oil `putIn` Bag
  Lighter `putIn` Bag
  goTo FireRoom
  cmd "make exit key"
  eM <- elementMap <$> get
  acc "make success" $ (eM ! ExitKey == Bag) && (eM ! Furnace == H FireRoom)

  put initialGame
  Clay `putIn` Bag
  GasZ `putIn` Bag
  goTo GeneratorRoom
  cmd "turn on generator"
  goTo ChemistryLab
  cmd "make mold"
  eM <- elementMap <$> get
  acc "make success mold" $ (eM ! Mold == Bag) && (eM ! ChemicalChamber == H ChemistryLab)

  put initialGame
  acc "make fail" =<< cmdF "make exit key"

testRunSwitch = do
  put initialGame
  goTo GeneratorRoom
  cmd "turn on generator"
  eL <- electricity <$> get
  acc "turn on success" $ eL == True

  goTo ChemistryLab
  acc "turn off fail" =<< cmdF "turn off generator"

  goTo GeneratorRoom
  cmd "turn off generator"
  eL <- electricity <$> get
  acc "turn off success" $ eL == False
  
  goTo ChemistryLab
  acc "turn on fail" =<< cmdF "turn on generator"

testRunMovement = do
  put initialGame
  cmd "go north"
  p <- player <$> get
  acc "move success" $ p == Pool

  put initialGame
  acc "move fail" =<< cmdF "go south"

  goTo LivingArea
  putIn ExitKey Bag
  cmd "go south"
  p <- player <$> get
  acc "move success exit" $ p == Exit

  goTo MasterBedroom
  cmd "hide under blue table"
  acc "move fail visibility" =<< cmdF "go south"

  cmd "unhide"
  cmd "go north"
  p <- player <$> get
  acc "move success visibility" $ p == BathroomMasterBedroom

  put initialGame
  goTo Pool
  acc "move fail bound" =<< cmdF "go north"

main = do
  runComp initialGame testRunBag 
  runComp initialGame testRunHide
  runComp initialGame testRunMake
  runComp initialGame testRunSwitch
  runComp initialGame testRunMovement
  return ()



