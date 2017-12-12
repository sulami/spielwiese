#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc

module Main where

import           Data.List  (nub)
import           Data.Maybe (fromMaybe)

type Pair = (Int, [Int])

main :: IO ()
main = do
  input <- zip [0..] . map connections . lines <$> getContents
  -- mapM_ print input
  print . length . nub $ graph 0 input

connections :: String -> [Int]
connections = map read . drop 2 . words . filter (/= ',')

graph :: Int -> [Pair] -> [Int]
graph _ [] = []
graph n xs = concat [ n : graph (fst x)  (filter ((/= n) . fst) xs) |
                      x <- [ (y, fromMaybe [] (lookup y xs)) |
                             y <- fromMaybe [] (lookup n xs) ] ]
