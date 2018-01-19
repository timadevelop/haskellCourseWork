module Room
where

import Object

data Room = Room {
    roomName :: String
  , takenPositions :: [Position]
  , objectsInside :: [Object]
} deriving (Show)

insertObject :: Object -> Room -> Room
insertObject obj room =
  if elem obj (objectsInside room) -- || not (isValidPosition  (getLocation obj))
    then room -- TODO error
    else
      let newObjects = obj:(objectsInside room)
          newTakenPositions = (getLocation obj):(takenPositions room)
      in room { takenPositions = newTakenPositions, objectsInside = newObjects }

      -- foldr' :: (Int -> a -> a -> a) -> a -> Int -> [a] -> a
      -- foldr' _ nv ni [] = nv
      -- foldr' op nv i l = op i (head l) (foldr' op nv (i + 1) (tail l))

      -- showRoom :: Room -> IO ()
      -- showRoom (Room name _ objects) =
        -- putStrLn ("Name: " ++ name ++ ". " ++ (show $ str))
      -- str :: [Object] -> [[Char]]
      -- str objs = foldr' helper [] 0 globalMatrix
      --   where
      --     helper ri row cacc = (foldr' subHelper [] 0 row ):cacc
      --       where
      --         subHelper :: Int -> Char -> [Char]
      --         subHelper ci e racc = if null $ findByPosition ri ci then e:racc else 'o':racc
      --           where
      --             findByPosition :: Int -> Int -> [Object]
      --             findByPosition ri ci = filter (\x -> (getLocation x) == (Position ri ci)) objs
