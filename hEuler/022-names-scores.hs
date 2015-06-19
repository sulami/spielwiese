-- Names scores
-- Problem 22
--
-- Using names.txt (right click and 'Save Link/Target As...'), a 46K text file
-- containing over five-thousand first names, begin by sorting it into
-- alphabetical order. Then working out the alphabetical value for each name,
-- multiply this value by its alphabetical position in the list to obtain a
-- name score.
--
-- For example, when the list is sorted into alphabetical order, COLIN, which
-- is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN
-- would obtain a score of 938 × 53 = 49714.
--
-- What is the total of all the name scores in the file?

{-# OPTIONS_GHC -O2 #-}

import Data.List

readNames :: String -> [String]
readNames s = read $ "[" ++ s ++ "]"

positionScores :: [a] -> [(a, Int)]
positionScores l = zip l [1..(length l)]

letterScores :: String -> Int
letterScores = foldl (\r e -> r + length ['A'..e]) 0

combineScores :: (Num a) => [(a, a)] -> [a]
combineScores = map (\(a,b) -> a * b)

process :: String -> Int
process s = sum $ combineScores $ positionScores $ map letterScores $ sort
            $ readNames s

main = do input <- readFile "022.input"
          print $ process input

