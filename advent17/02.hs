#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc

module Main where

type ChecksumF = [Int] -> Int
type CheckSum = Int

main :: IO ()
main = do
  input <- map (map read . words) . lines <$> getContents
  print $ checksum minMax input
  print $ checksum evenDivide input

checksum :: ChecksumF -> [[Int]] -> CheckSum
checksum f = sum . map f

minMax :: ChecksumF
minMax line = maximum line - minimum line

evenDivide :: ChecksumF
evenDivide line = head [x `div` y | x <- line, y <- line, x `mod` y == 0]
