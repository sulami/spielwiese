#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc

module Main where

import           Data.List (nub, sort)

main :: IO ()
main = do
  input <- map words . lines <$> getContents
  print . length $ filter isValid input
  print . length $ filter isSecure input

isValid :: [String] -> Bool
isValid phrase = phrase == nub phrase

isSecure :: [String] -> Bool
isSecure phrase = let ws = map sort phrase in ws == nub ws
