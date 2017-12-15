#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc

module Main where

import           Data.Char (intToDigit)
import           Numeric   (showIntAtBase)

main :: IO ()
main = do
  input <- map read . lines <$> getContents :: IO [Int]
  let a = generator 16807 $ head input
      b = generator 48271 $ last input
  print . length . filter id . take 40000000 $ zipWith (==) a b

generator :: Int -> Int -> [String]
generator fac = map (drop 16 . showBinary) . iterate (generate fac)

generate :: Int -> Int -> Int
generate fac base = (base * fac) `rem` 2147483647

showBinary :: Int -> String
showBinary x = let bin = showIntAtBase 2 intToDigit x ""
                   prefix = replicate (32 - length bin) '0'
               in prefix ++ bin
