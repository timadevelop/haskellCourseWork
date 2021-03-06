
import Commands

import InitialGameState (initialGameState)
import GameState

import ListUtils (removeSuccessiveDups)
import Utils


-- some texts
endText :: String
endText = "\nYou found a way to open the car and break through the gates of a mystical city. \
\ \n\nA day later you were already in your hometown, you were met by a family, and you threw the car on the road. \
\ \n\nA week later, the police found your luggage and returned it to you, and that robber was punished. \
\ \n\nYou told me what happened to your wife and thought: \
\ \n\"My next trip will necessarily take place in a small town called Blank Town\" \
\ \n\nWho is this Stan eventually?\n\nThe End.\n\n"

startText :: String
startText = "\nHey. Your name is Chris, 2 hours ago you were a fellow traveler in the car that was moving to your home town.\
\ \nYou got into this city by accident, after the driver drove you out of the car and left with your luggage.\
\ \nYou must get out of this place and get home at any cost. \
\ \n\nType \"help\" to see what you can do"


runGame :: IO ()
runGame = eval initialGameState
    where
    eval :: GameState -> IO ()
    eval lastGameState =
        do
          putStr "\n"
          putStrLn (msg lastGameState)
          putStr "\n------------- > "
          command <- getLine
          if command == "quit"
            then
              putStrLn "Failed"
              else
                case execute (trim $ removeSuccessiveDups ' ' command) lastGameState of
                  Utils.Error str -> (eval (setMsg str lastGameState))
                  Utils.Just newGameState -- execute returns new gamestate
                      | isEnd newGameState -> putStrLn ((msg newGameState) ++ "\n" ++ endText)
                      | otherwise -> eval newGameState


main = do
    putStr startText
    runGame
