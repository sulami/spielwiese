#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc

module Main where

import Data.List (nub)

main :: IO ()
main = do
  input <- map words . lines <$> getContents
  print . length $ filter isValid input

isValid :: [String] -> Bool
isValid phrase = phrase == nub phrase
