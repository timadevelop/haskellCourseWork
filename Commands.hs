module Commands
    (callCommand
    ) where


import Data.List.Split

import Data.Char

import ListUtils (removeSuccessiveDups)
import Utils (trim)
import Position
import Object
import Room
import GameState
import Utils
import Object

callCommand :: String -> [String] -> GameState -> JustOrError GameState String
callCommand [] _ _ = Utils.Error "No cmd"

callCommand "go" [] _ = Utils.Error "Where do you want to go? (left, right, up, down)?"
callCommand "go" ([]:_) _ = Utils.Error "Where do you want to go? (left, right, up, down)?"
callCommand "go" (arg:args) gs --Utils.Just $ (movePlayer (getDirectionVector arg) gs)
  | isDigit (head arg) && not (null args) =
      if digitToInt (head arg) == 1
        then callCommand "go" args gs
        else
          case callCommand "go" args gs of
            Utils.Error s -> Utils.Just gs
            Utils.Just newGs -> let newArgs :: [String]
                                    newArgs = [(intToDigit (digitToInt (head arg) - 1))]:[(head args)]
                                in (callCommand "go" newArgs newGs)
  | otherwise =
    let oldGs = (movePlayer (getDirectionVector arg) gs)
        lastMsg = (msg oldGs)
    in
    case (callCommand "look" [] oldGs) of
      Utils.Error s -> Utils.Just oldGs
      Utils.Just newGs -> Utils.Just $ setMsg (lastMsg ++ "\n" ++ replicate 15 '-' ++ "\n" ++ (msg newGs)) newGs
callCommand "p" args gs = callCommand "position" args gs
callCommand "position" _ gs = Utils.Just (setMsg ("You are in " ++ (show $ currentPlayerPosition gs)) gs)
callCommand "l" args gs = callCommand "look" args gs
callCommand "look" _ gs =
  let
    distance obj = distanceBetween (getLocation obj) (getLocation $ getCurrentPlayer gs) :: Position
    helper obj = (getObjectName obj) ++ " is on " ++ (getDirectionWord (distance obj))
    aroundObjectsStr = foldr (\objStr acc -> "\n" ++ objStr ++ acc) [] (map helper (getObjectsAround (getCurrentPlayer gs) gs)) :: String
    sees = if null aroundObjectsStr then "only land around you. Try to go somewhere." else "following objects: " ++ aroundObjectsStr :: String
    newMsg = "You are in a room " ++ (roomName $ getCurrentRoom gs) ++ " and you see " ++ sees :: String
  in Utils.Just (setMsg newMsg gs)

-- callCommand "use" ("car":side:_)

callCommand "use" [] _ = Utils.Error "What do you want to use?"
callCommand "use" ("car":[]) _ = Utils.Error "Specify the direction to car location"
callCommand "use" ("car":side:_) gs
  | elem side sides = useCar side gs
  | otherwise = Utils.Error "Specify the proper direction of car location"
callCommand "take" [] _ = Utils.Error "What do you want to take?"
callCommand "take" ("object":"on":"the":side:_) gs = takeObjectOnSide side gs
callCommand "take" (side:_) gs
  | elem side sides = takeObjectOnSide side gs
  | otherwise = Utils.Error "Use some direction to take object (for example get right -> get right object)"
-- callCommand "take" [] _ = Utils.Error "give some direction"
callCommand "i" args gs = callCommand "inventory" args gs
callCommand "inventory" [] gs =
  let newMsg = objectsToString (inventory $ getCurrentPlayer gs)  (\o -> "\n" ++ (getObjectName o)) :: String
  in Utils.Just (setMsg newMsg gs)
callCommand "inventory" ("!!":(i:_):_) gs =
  if (not $ isDigit i) || digitToInt i >= (length $ inventory $ getCurrentPlayer gs)
    then Utils.Just (setMsg "There is no object on this position in inventory" gs)
    else
      let
        format = (\o -> (getObjectName o) ++ " contents:\n" ++ "\"" ++ (content o) ++ "\"")
        newMsg = objectsToString [(inventory $ getCurrentPlayer gs) !! (digitToInt i)] format :: String
      in Utils.Just (setMsg newMsg gs)
callCommand "h" args gs = callCommand "help" args gs
callCommand "help" _ _ = Utils.Error "You can run any of the following commands: \
\ \n p or position                                - this command will show your position \
\ \n go <right | left | up | down>                - go in the chosen direction \
\ \n go <1 | 2 | .. 9> <right | left | up | down> - go <N> times in the chosen direction \
\ \n l or look                                    - to look what do you see around you \
\ \n take <right | left | up | down>              - to take the object from the selected side \
\ \n i or inventory                               - to display your inventory \
\ \n <inventory | i> !! <0 | 1 | .. 9>            - to see the content of N-th element from your inventory \
\ \n use car <right | left | up | down>           - to use a car for escaping \
\ \n h or help                                    - to see this message again"
callCommand _ _ _ = Utils.Error "You can't do it, Sorry."



takeObjectOnSide :: String -> GameState -> JustOrError GameState String
takeObjectOnSide [] _ = Utils.Error "tell me there from you want to take some object."
takeObjectOnSide side gs =
  let location = sumPositions (currentPlayerPosition gs) (getDirectionVector side) :: Position
      foundObjs = filter (\obj -> isSmall obj && (getLocation obj) == location) (objectsInside $ getCurrentRoom gs) :: [Object]
  in
    if null foundObjs
      then Utils.Error "There is no object you can take to inventory."
      else Utils.Just $ insertToInventory (head foundObjs) (GameState.removeObj (head foundObjs) gs)

useCar :: String -> GameState -> JustOrError GameState String
useCar side gs =
  let location = sumPositions (currentPlayerPosition gs) (getDirectionVector side) :: Position
      currentInventory = inventory $ getCurrentPlayer gs  :: [Object]
      keys = map content (filter isKey currentInventory) :: [String]
      foundCars = filter (\obj -> isCar obj && (getLocation obj) == location) (objectsInside $ getCurrentRoom gs) :: [Object]
  in
    if null foundCars
      then Utils.Error "There is no car in this direction"
      else if not $ null $ filter (\obj -> elem (carKey obj) keys) foundCars
        then Utils.Just $ win gs
        else Utils.Error "You have no key for this car."

sides = ["left", "right", "up", "down"]


movePlayer :: Position -> GameState -> GameState
movePlayer (Position 0 0) gs = setMsg "what? You need to specify the drection to go" gs
movePlayer direction gs = GameState.moveObject direction (getCurrentPlayer gs) gs
