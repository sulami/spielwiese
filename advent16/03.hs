module Main where

import           Data.List (sort)

isValid :: [Int] -> Bool
isValid sides = let sorted = sort sides
                in sum (init sorted) > last sorted

main :: IO ()
main = do
  indata <- map (map read . words :: String -> [Int]) . lines <$>
    readFile "03.input"
  print . length $ filter isValid indata
