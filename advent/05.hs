module Main where

import           Data.List (isInfixOf)

isNice :: String -> Bool
isNice s = length (filter (`elem` "aeiou") s) >= 3
        && any (`isInfixOf` s) (map (replicate 2) ['a'..'z'])
        && all (not . (`isInfixOf` s)) ["ab","cd","pq","xy"]

isNice2 :: String -> Bool
isNice2 s = prop1 s && prop2 s
  where
    prop1 :: String -> Bool
    prop1 (x:y:z) = [x,y] `isInfixOf` z || prop1 (y:z)
    prop1 _       = False

    prop2 :: String -> Bool
    prop2 (a:b:c:d) = a == c || prop2 (b:c:d)
    prop2 _         = False

main = do
  indata <- lines <$> readFile "05"
  print . length $ filter isNice indata
  print . length $ filter isNice2 indata

