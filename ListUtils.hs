module ListUtils
where

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newElement xs = take n xs ++ [newElement] ++ drop (n + 1) xs

getElem :: (a -> Bool) -> [a] -> a
getElem p l = head $ filter p l

removeSuccessiveDups :: Eq a => [a] -> [a]
removeSuccessiveDups [] = []
removeSuccessiveDups (x:xs) = x:removeSuccessiveDups (dropWhile (\e -> e == x) xs)
