module InitialGameState
    ( initialGameState
    ) where

import Data.List.Split

import ListUtils (removeSuccessiveDups)
import Utils (trim)
import Position
import Object
import Room
import GameState
import Utils
import Object

{-
1,2,3 are hints
P is a player
C is a car
L is a library
K is a key for for a car (11,11)
k - inValid Keys

  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4
0 P 1 o o o o o o o o o o o o o
1 o o o o o o o o o o o o o o o
2 o C 2 o o o o o o o o o o o o
3 o o o o o o o o o o o o o o o
4 o o o o o o o o o o o o o o o
5 o o o o o o o o o o o o o o o
6 o o o o o o o o o o o o o o o
7 o o o o o o o o o o o o o o o
8 o o o o o o o o o o o o o o o
9 o o o o o o o o o 0 o k o o o
0 o o o o o o o o o o L o o o o
1 o o o o o o o o o o o K o o o
2 o o o o o o o o o o o o o o o
3 o o o o o o o o o o o o o o o
4 o o o o o o o o o o o o o o o

-- find hint 1 -> find car -> try to use car -> find hint2 ->
-- find library -> find proper key near library ->
-- go to the car -> use car -> end of the game
-}
player = Player "Vlad" 20 [] (Position 0 0)


hint1 = Small "Hint 1" "you must escape from here!!! I tried to escape in the most strange ways, \
\\nbut in the end I realized that the best way was to find a car and break through the huge gates of this city,\
\\nI think that car is still somewhere near. Try go bottom. You need a" (Position 0 1)

car = Car "Mercedes-Benz E220" "d12j1cojnIqw" (Position 2 1)
hint2 = Small "Hint 2" "the key is hidden near the entrance to the library. \
\\nLibrary is located neer bottom right area of this town" (Position 2 2)

libraryDoor = Door "" "Library" (Position 10 10) -- library
key = Small "Key" "d12j1cojnIqw" (Position 11 11) -- valid key :D
key2 = Small "Key" "d12j1cojnIqv" (Position 9 11) -- invalid key :)


hint0 = Small "Hint 0" "ello, my name is Stan, this diary has a lot of secrets of this town, \
\\nmany pages of my diary are lost, but if you read th" (Position 9 9)

food = Small "Food" "Meat" (Position 2 2)


firstRoomObjects = [player, hint1, car, hint2, libraryDoor, key, key2, hint0, food]


initialRooms = [Room "Home" firstRoomObjects
                ,  Room "Yard" []
                , Room "Street" []
                , Room "Shop" []]

initialGameState :: GameState
initialGameState = GameState 0 initialRooms "" False
