module ListUtils
where

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newElement xs = take n xs ++ [newElement] ++ drop (n + 1) xs

getElem :: (a -> Bool) -> [a] -> a
getElem p l = head $ filter p l

removeSuccessiveDups :: Eq a => a -> [a] -> [a]
removeSuccessiveDups _ [] = []
removeSuccessiveDups sym (x:xs) = x:removeSuccessiveDups sym (dropWhile (\e -> e == x && e == sym) xs)
