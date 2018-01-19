module Object
where
-- import Position
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
              deriving (Show, Eq)

-- Position
data Position = Position Int Int deriving (Show, Eq)

sumPositions :: Position -> Position -> Position
sumPositions (Position x1 y1) (Position x2 y2) = Position (x1 + x2) (y1 + y2)

instance Ord Position where
  (<=) (Position x1 y1) (Position x2 y2) = x1 <= x2 && y1 <= x2

--- Located
class Located a where
    getLocation :: a -> Position

instance Located Object where
    getLocation (Car _ _ p) = p
    getLocation (Door _ _ p) = p
    getLocation (Player _ _ _ p) = p

--- Movable
class (Located a) => Movable a where
    setLocation :: Position -> a -> a
    move :: Position -> a -> (Position -> Bool )-> Prelude.Maybe a
--
instance Movable Object where
    setLocation newPosition obj@(Car _ _ _) = obj {carPosition = newPosition}
    move vector obj@(Car _ _ lastPosition) p =
      let newPosition = sumPositions lastPosition vector
      in if p newPosition
        then Prelude.Just (setLocation newPosition obj) -- set new ps
        else Prelude.Nothing  -- Error "You cannot go there" -- do nothing
