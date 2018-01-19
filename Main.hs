import Data.List.Split

import Object
import Room
import GameState
import Utils
import Object

x = Land
car1 = Car "Mercedes-Benz E220" "d12j1cojnIqw" (Position 1 2)
door1 = Door "adfasfad24ald" "somewhere" (Position 1 3)
player = Player "Vlad" 20 [] (Position 0 0)
firstRoomObjects = [car1, player]

initialRooms = [Room "Home" (map getLocation firstRoomObjects) firstRoomObjects
                ,  Room "Yard" [] []
                , Room "Street" [] []
                , Room "Shop" [] []]

initialGameState :: GameState
initialGameState = GameState 0 initialRooms "" False

runGame :: IO ()
runGame = eval initialGameState
    where
    eval :: GameState -> IO ()
    eval lastGameState =
        do
          putStrLn (msg lastGameState)
          putStr "command> "
          command <- getLine
          if command == "quit"
            then
              putStrLn "End of the game"
              else
                case execute command lastGameState of
                  Utils.Error str -> (eval (setMsg str lastGameState))
                  Utils.Just newGameState -- execute returns new gamestate
                      | isEnd newGameState -> putStrLn "End of the game"
                      | otherwise -> eval newGameState
-- break :: (a -> Bool) -> [a] -> ([a],[a])
-- break p l = (takeWhile q l, dropWhile q l)
-- where q x = not (p x)
--
-- splitString :: String -> Char -> [String]
-- splitString [] _ = []
-- splitString (x:xs) c
--   | x == c = splitString xs c
--   | otherwise = x:splitString

execute :: String -> GameState -> JustOrError GameState String
execute "" _ = Utils.Error "Pass the comand"
execute cmd gs =
  let command = (splitOn " " cmd)
  in callCommand (head command) (tail command) gs
-- execute _ _ = Utils.Error "You can't do it, Sorry."

callCommand :: String -> [String] -> GameState -> JustOrError GameState String
callCommand [] _ _ = Utils.Error "No cmd"
callCommand "go" [] _ = Utils.Error "Where do you want to go? (left, right, up, down)?"
callCommand "go" (arg:args) gs = Utils.Just (setMsg ("You are going " ++ arg) gs)

callCommand _ _ _ = Utils.Error "You can't do it, Sorry."

  -- Utils.Just (setMsg ("Hello from function, You called " ++ cmd ++ " function") gs)

-- move (Position 1 2) car1 (\p -> isValidPosition p initialGameState) -> Noting, Just newElement

main = do
    -- g <- getStdGen
    runGame
