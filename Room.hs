module Room
where
import ListUtils
import Data.List
import Position
import Object

data Room = Room {
    roomName :: String
  -- , takenPositions :: [Position]
  , objectsInside :: [Object]
} deriving (Show, Eq)

getPlayer :: Room -> Object
getPlayer room = head $ filter isPlayer (objectsInside room)

insertToPlayerInventory :: Object -> Room -> Room
insertToPlayerInventory obj room = replaceObj (getPlayer room) (insertToInventoryOf (getPlayer room) obj) room


takenPositions :: Room -> [Position]
takenPositions room = map (\obj -> (getLocation obj)) (objectsInside room)

insertObject :: Object -> Room -> Room
insertObject obj room =
  if elem obj (objectsInside room) -- || not (isValidPosition  (getLocation obj))
    then room -- TODO error
    else
      let newObjects = obj:(objectsInside room)
      in room { objectsInside = newObjects }

replaceObj :: Object -> Object -> Room -> Room
replaceObj obj newObj room =
  case elemIndex obj (objectsInside room) of
    Prelude.Nothing -> insertObject obj room
    Prelude.Just index -> room { objectsInside = replaceNth index newObj (objectsInside room) }

removeObj :: Object -> Room -> Room
removeObj obj room = room { objectsInside = (filter (/=obj) (objectsInside room)) }



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
