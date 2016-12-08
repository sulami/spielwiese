module Main where

import           Data.List (sort, transpose)

isValid :: [Int] -> Bool
isValid sides = let sorted = sort sides
                in sum (init sorted) > last sorted

tripleTranspose :: [[a]] -> [[a]]
tripleTranspose [] = []
tripleTranspose l  = transpose (take 3 l) ++ tripleTranspose (drop 3 l)

main :: IO ()
main = do
  indata <- lines <$> readFile "03.input"
  let part_one = map (map read . words :: String -> [Int]) indata
      part_two = tripleTranspose part_one
  print . length $ filter isValid part_one
  print . length $ filter isValid part_two
