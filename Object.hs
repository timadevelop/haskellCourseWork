module Object
where
import Position
-- import GameState
--- object type
data Object =  Land
              | Player {playerName :: String
                    , playerAge :: Int
                    , inventory :: [Object]
                    , playerPosition :: Position
                    }
              | Car {carName :: String
                    , carKey :: String
                    , carPosition :: Position
                    }
              | Door {doorKey :: String
                    , destination :: String
                    , doorPosition :: Position
                    }
              | Small {title :: String
                    , content :: String
                    , smallPosition :: Position
                    }
              deriving (Show, Eq)


insertToInventoryOf :: Object -> Object -> Object
insertToInventoryOf player@(Player _ _ inv _) obj@(Small _ _ _) =  player { inventory = obj:inv }

isKey :: Object -> Bool
isKey (Small "Key" _ _) = True
isKey _ = False

isSmall :: Object -> Bool
isSmall (Small _ _ _) = True
isSmall _ = False

isPlayer :: Object -> Bool
isPlayer (Player _ _ _ _) = True
isPlayer _ = False

isCar :: Object -> Bool
isCar (Car _ _ _) = True
isCar _ = False

getObjectName :: Object -> String
getObjectName (Player name _ _ _) = "Player with name " ++ name
getObjectName (Car name _ _) = "Car with name " ++ name
getObjectName (Door _ dest _) = "Door to " ++ dest
getObjectName (Small title _ _) = title

objectsToString :: [Object] -> (Object -> String) -> String
objectsToString objs toString = foldr (\objStr acc ->  objStr ++ acc) [] (map toString objs)

--- Located
class Located a where
    getLocation :: a -> Position

instance Located Object where
    getLocation (Car _ _ p) = p
    getLocation (Door _ _ p) = p
    getLocation (Player _ _ _ p) = p
    getLocation (Small _ _ p) = p

--- Movable
class (Located a) => Movable a where
    setLocation :: Position -> a -> a
    move :: Position -> a -> (Position -> Bool )-> Prelude.Maybe a
--
instance Movable Object where
    setLocation newPosition obj@(Car _ _ _) = obj {carPosition = newPosition}
    setLocation newPosition obj@(Player _ _ _ _) = obj {playerPosition = newPosition}
    move vector obj p =
      let newPosition = sumPositions (getLocation obj) vector
      in if p newPosition
        then Prelude.Just (setLocation newPosition obj) -- set new ps
        else Prelude.Nothing  -- Error "You cannot go there" -- do nothing
