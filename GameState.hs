module GameState
where

import Data.List
import Position
import ListUtils (replaceNth)
import Object
import Room

globalBounds = ((Position 0 0), (Position 15 15))
-- globalMatrix = [ row | row <- replicate 15 ['a'..'z'] ]

data GameState = GameState {
   currentRoomIndex :: Int
   , rooms :: [Room]
   , msg :: String -- msg from last cmd call
   , isEnd :: Bool
} deriving (Show)

win :: GameState -> GameState
win gs = gs { isEnd = True }

getRoom :: GameState -> Int -> Room
getRoom gs i = (rooms gs) !! i

getCurrentRoom :: GameState -> Room
getCurrentRoom gs = getRoom gs (currentRoomIndex gs)

getCurrentPlayer :: GameState -> Object
getCurrentPlayer gs = Room.getPlayer $ getCurrentRoom gs

currentPlayerPosition :: GameState -> Position
currentPlayerPosition gs = getLocation $ getCurrentPlayer gs

setMsg :: String -> GameState -> GameState
setMsg str gs = gs { msg = str }

replaceRoom :: Room -> Room -> GameState -> GameState
replaceRoom room newRoom gs =
  case elemIndex room (rooms gs) of
    Prelude.Nothing -> gs
    Prelude.Just index -> gs { rooms = replaceNth index newRoom (rooms gs) }

createObject :: Object -> GameState -> GameState
createObject obj gs = gs { rooms = replaceNth (currentRoomIndex gs) (insertObject obj (getCurrentRoom gs)) (rooms gs) }

moveObject :: Position -> Object -> GameState -> GameState
moveObject vector obj gs =
  case Object.move vector obj (\pos -> isValidPosition pos gs) of
    Prelude.Nothing -> setMsg "Cannot go there" gs
    Prelude.Just newObj -> -- update GameState
      let newGameState = replaceRoom (getCurrentRoom gs) (Room.replaceObj obj newObj (getCurrentRoom gs)) gs
      in (setMsg ("Ok, Your position is " ++ (show $ getLocation $ getCurrentPlayer newGameState)) newGameState)

-- get all objects around passed objects
-- [

-- [1 1 1 1 1]
-- [1 0 0 0 1]
-- [1 0 p 0 1] - if p is a player - we need take all zeros we have
-- [1 0 0 0 1]
-- [1 1 1 1 1]
-- ]
getObjectsAround :: Object -> GameState -> [Object]
getObjectsAround o gs =
   filter (\obj -> obj /= o && isAround (getLocation obj) (getLocation o)) (objectsInside $ getCurrentRoom gs)
    where
      distance = 1
      isAround :: Position -> Position -> Bool
      isAround (Position oy ox) (Position py px) =
        (ox <= (px + distance) && ox >= (px - distance)) &&
        (oy <= (py + distance) && oy >= (py - distance))


-- is Valid Position in the GameState
isValidPosition :: Position -> GameState -> Bool
isValidPosition p state =
  p >= (fst globalBounds) &&
  p <= (snd globalBounds) &&
  not (elem p (takenPositions $ getCurrentRoom state ))

test :: GameState -> GameState
test gs = GameState.moveObject (Position 4 2) (head (objectsInside (getCurrentRoom gs))) gs

-- remove object from current room
removeObj :: Object -> GameState -> GameState
removeObj obj gs = replaceRoom (getCurrentRoom gs) (Room.removeObj obj (getCurrentRoom gs)) gs

-- insert object from current room
insertToInventory :: Object -> GameState -> GameState
insertToInventory obj@(Small _ _ _) gs =
  let newGs = replaceRoom (getCurrentRoom gs) (Room.insertToPlayerInventory obj (getCurrentRoom gs)) gs
  in setMsg "The item is in yoor inventory" newGs
insertToInventory _ gs = setMsg "You can take only small objects such as hints or keys" gs
