#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc

module Main where

main :: IO ()
main = do
  input <- map (map read . words) . lines <$> getContents
  print $ checksum minMax input
  print $ checksum evenDivide input

checksum :: ([Int] -> Int) -> [[Int]] -> Int
checksum f = sum . map f

minMax :: [Int] -> Int
minMax line = maximum line - minimum line

evenDivide :: [Int] -> Int
evenDivide line = head [x `div` y | x <- line, y <- line, x `mod` y == 0]
