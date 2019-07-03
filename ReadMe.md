Title         : Murder Mansion

[TITLE]

# Game Idea

## Description

Get out of the house whilst avoiding the murderer. The only way
leave the house is through the main exit. The main exit 
only opens with a main key. Unfortunately, the main key is
with the murderer. Hence you need to make your own main key by
finding combination of elements to make the main key. Explore
the house, gather items to solve problems and escape the murderer.

## Game elements

- You have a bag with you in which you can carry items that you find in the house.
- You can make the main key if you have mold and Steel in your bag and a furnace is in the vicinity.
- The furnace can be started if you have oil and lighter in your bag.
- You can make the mold using clay and gas z if a chemical chamber is in the vicinity.
- The chemical chamber needs electricity to run.
- The generator can be used to turn on the electricity in the house.
- Steel can be found the storage room.
- The storage room needs a storage key to enter.

## The Map

- The map consists of multiple rooms with different purposes.
- Different rooms have different elements hidden.
- The player has to explore all the rooms, find/make items and escape the house.

# Implementation Details

## Modules

The game consists of 17 modules in total.

1. Main
2. Game
3. Game.Internal
4. Game.Map
5. Asset
6. Asset.Internal
7. Asset.Element
8. Asset.Location
9. Asset.Message
10. Command
11. Command.Bag
12. Command.Hide
13. Command.Make
14. Command.Movement
15. Command.Switch
16. Player
17. Murderer

## Module description

### Main

This is the main execution point of the program.

### Game

This module contains the main game loop for execution.
This module is responsible for setting up the game and executing it.
This the top level module.

### Game.Internal

This is the internal game module that states how a game looks
and has the initial state of the game.

### Game.Map

This module contains the default map of the game,
that is, locations mapped to coordinates.

### Asset

This module re-exports every module in Asset.\* 

### Asset.Internal

This module defines a few classes that are required for any asset.
The classes defined are HasInfo, HasStringID, IsPickable, and ResponseMessage.

### Asset.Element

This module contains the elements in the game. Things like 'generator',
'blue table' etc are defined in this module.

### Asset.Location

This module contains valid locations in the game. Things like 'living room' and 'pool' are defined in this module.

### Asset.Message

This module contains types abstracting any kind of information.
Errors and a few game instruction are defined here.

### Command

This module is exports the main parser and all game command data types.

### Command.Bag

This module contains commands + parser that interacts with a bag.

Examples,

- parse "pick up x" = PickUp \<$\> toAsset x
- parse "drop x" = Drop \<$\> toAsset x

### Command.Hide

This module contains commands + parser that hide/show the player.

Examples,

- parse "hide under x" = Hide \<$\> toAsset x
- parse "unhide" = Just Unhide

### Command.Make

This module contains commands + parser to make a certain item is possible.

Examples,

- parse "make x" = Make \<$\> toAsset x

### Command.Movement

This module contains commands + parser for navigation. 

Examples,

- parse "go north" = Just North
- parse "go east" = Just East
- parse "go up" = Just Up

### Command.Switch

This module contains commands + parser for turning on/off things. 

Examples,

- parse "turn on x" = TurnOn \<$\> toAsset x

### Player

This module contains the actions that should occur during the player's turn.

### Murderer

This module exports a simple AI murderer who randomly moves.

## Screenshots

![game-run-sample]

![game-run-sample-3]

![game-run-sample-2]

[game-run-sample]: images/game-run-sample.PNG "game-run-sample" { width:auto; max-width:90% }

[game-run-sample-3]: images/game-run-sample-3.PNG "game-run-sample-3" { width:auto; max-width:90% }

[game-run-sample-2]: images/game-run-sample-2.PNG "game-run-sample-2" { width:auto; max-width:90% }



