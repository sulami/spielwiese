module Main where

import Data.List (minimumBy, nub, sort, subsequences)
import Data.Ord (comparing)

main = do
  containers <- sort . map read . lines <$> readFile "17.input" :: IO [Int]
  let combos = filter (\l -> sum l == 150) $ subsequences containers
  print $ length combos
  let mini = length $ minimumBy (comparing length) combos
  print . length $ filter (\l -> length l == mini) combos

