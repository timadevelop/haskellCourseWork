module Position
where

-- Position
data Position = Position Int Int deriving (Show, Eq)

-- row
r :: Position -> Int
r (Position r _) = r

-- column
c :: Position -> Int
c (Position _ c) = c

-- some helpers
sumPositions :: Position -> Position -> Position
sumPositions (Position r1 c1) (Position r2 c2) = Position (r1 + r2) (c1 + c2)

distanceBetween :: Position -> Position -> Position
distanceBetween (Position r1 c1) (Position r2 c2) = Position (r1 - r2) (c1 - c2)

-- Ord typeclass for Position
instance Ord Position where
  (<=) (Position r1 c1) (Position r2 c2) = r1 <= r2 && c1 <= r2


-- maybe it's better to use associative list
getDirectionVector :: String -> Position
getDirectionVector "left" = Position 0 (-1)
getDirectionVector "right" = Position 0 1
getDirectionVector "up" = Position (-1) 0
getDirectionVector "down" = Position 1 0
getDirectionVector _ = Position 0 0

getDirectionWord :: Position -> String
getDirectionWord (Position 0 (-1)) = "left"
getDirectionWord (Position 0 1) = "right"
getDirectionWord (Position (-1) 0) =  "up"
getDirectionWord (Position 1 0) = "down"
getDirectionWord (Position 0 0) = "under you"
getDirectionWord (Position (-1) (-1)) = "left up corner"
getDirectionWord (Position (-1) 1) = "right up corner"
getDirectionWord (Position 1 (-1)) = "left bottom corner"
getDirectionWord (Position 1 1) = "right bottom corner"
getDirectionWord _ = "undefined"

sides = ["left", "right", "up", "down"]
