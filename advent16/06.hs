module Main where

import           Data.List (elemIndices, maximumBy, nub, transpose)
import           Data.Ord  (comparing)

recover :: [String] -> String
recover input = let cols = transpose input
                in concatMap recoverChar cols
  where
    recoverChar :: [Char] -> [Char]
    recoverChar xs = [maximumBy (comparing (numOccurences xs)) $ nub xs]

    numOccurences :: [Char] -> Char -> Int
    numOccurences cl c = length $ elemIndices c cl

main :: IO ()
main = do
  indata <- lines <$> readFile "06.input"
  print $ recover indata
