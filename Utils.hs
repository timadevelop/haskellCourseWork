module Utils
where

import Data.Char (isSpace)

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace
data JustOrError a b = Just a | Error b
