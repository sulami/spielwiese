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
      pickyA = map showBinary $ filter (\x -> x `mod` 4 == 0) a
      pickyB = map showBinary $ filter (\x -> x `mod` 8 == 0) b
  print . length . filter id . take 40000000 $ zipWith (==) a b
  print . length . filter id . take 5000000 $ zipWith (==) pickyA pickyB

generator :: Int -> Int -> [Int]
generator fac = iterate (generate fac)

generate :: Int -> Int -> Int
generate fac base = (base * fac) `rem` 2147483647

showBinary :: Int -> String
showBinary x = let bin = showIntAtBase 2 intToDigit x ""
                   prefix = replicate (32 - length bin) '0'
               in drop 16 $ prefix ++ bin
