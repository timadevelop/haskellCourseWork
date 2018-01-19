module GameState
where

import ListUtils
import Object
import Room

globalBounds = ((Position 0 0), (Position 15 15))
globalMatrix = [ row | row <- replicate 15 ['a'..'z'] ]

data GameState = GameState {
   currentRoomIndex :: Int
   , rooms :: [Room]
   , msg :: String
   , isEnd :: Bool
} deriving (Show)

getRoom :: GameState -> Int -> Room
getRoom gs i = (rooms gs) !! i

getCurrentRoom :: GameState -> Room
getCurrentRoom gs = getRoom gs (currentRoomIndex gs)

setMsg :: String -> GameState -> GameState
setMsg str gs = gs { msg = str }

endGame :: GameState -> GameState
endGame gs = gs { isEnd = True }

createObject :: Object -> GameState -> GameState
createObject obj gs = gs { rooms = replaceNth (currentRoomIndex gs) (insertObject obj (getCurrentRoom gs)) (rooms gs) }

isValidPosition :: Position -> GameState -> Bool
isValidPosition p state = p >= (fst globalBounds) &&
                    p <= (snd globalBounds) &&
                    not (elem p (takenPositions $ getCurrentRoom state ))
