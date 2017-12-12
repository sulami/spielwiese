#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc

module Main where

import           Data.List  (nub)
import           Data.Maybe (fromMaybe)

type Pair = (Int, [Int])

main :: IO ()
main = do
  input <- zip [0..] . map connections . lines <$> getContents
  print . length . nub $ graph 0 input
  print . length $ groups input

connections :: String -> [Int]
connections = map read . drop 2 . words . filter (/= ',')

graph :: Int -> [Pair] -> [Int]
graph _ [] = []
graph n xs = concat [ n : graph (fst x)  (filter ((/= n) . fst) xs) |
                      x <- [ (y, fromMaybe [] (lookup y xs)) |
                             y <- fromMaybe [] (lookup n xs) ] ]

groups :: [Pair] -> [[Int]]
groups [] = []
groups xs = let this = graph (fst $ head xs) xs
                rest = filter ((`notElem` this) . fst) xs
            in this : groups rest
