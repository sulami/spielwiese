#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc

module Main where

main :: IO ()
main = do
  input <- map (map read . words) . lines <$> getContents
  print $ checksum input

checksum :: [[Int]] -> Int
checksum = sum . map lineDiff

lineDiff :: [Int] -> Int
lineDiff line = maximum line - minimum line
