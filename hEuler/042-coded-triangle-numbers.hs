-- Coded triangle numbers
-- Problem 42
--
-- The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1);
-- so the first ten triangle numbers are:
--
-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
--
-- By converting each letter in a word to a number corresponding to its
-- alphabetical position and adding these values we form a word value. For
-- example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word
-- value is a triangle number then we shall call the word a triangle word.
--
-- Using words.txt (right click and 'Save Link/Target As...'), a 16K text file
-- containing nearly two-thousand common English words, how many are triangle
-- words?

{-# OPTIONS_GHC -O2 #-}

readWords :: String -> [String]
readWords s = read $ "[" ++ s ++ "]"

wordValue :: String -> Int
wordValue = foldl (\r e -> r + length ['A'..e]) 0

triangle :: [Int]
triangle = map round $ map fst $ iterate (\(a,c) -> (c/2 * (c+1), c+1)) (1,2)

risingElem :: (Ord a, Eq a) => [a] -> a -> Bool
risingElem l n = elem n (takeWhile (<= n) l)

main = do input <- readFile "042.input"
          print $ length $ filter (risingElem triangle) $ map wordValue
            $ readWords input

