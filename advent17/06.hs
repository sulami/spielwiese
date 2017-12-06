#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc

module Main where

import           Data.List  (elemIndex)
import           Data.Maybe (fromJust)

type Bank = [Int]

main :: IO ()
main = do
  input <- map read . words <$> getLine :: IO Bank
  print . (\x -> x - 1) . length $ findDupe [input]

findDupe :: [Bank] -> [Bank]
findDupe xs = let newBank = step $ head xs
              in if newBank `elem` xs
                 then newBank : xs
                 else findDupe $ newBank : xs

step :: Bank -> Bank
step bank = let maxValue = maximum bank
                zeroes = repeat 0
                size = length bank
                highest = fromJust $ elemIndex maxValue bank
                ones = replicate (highest + 1) 0 ++ replicate maxValue 1
                (h,t) = splitAt highest bank
                base = h ++ [0] ++ drop 1 t
            in foldr (zipWith (+) . take size . (++ zeroes)) base $ segment size ones

segment :: Int -> [a] -> [[a]]
segment _ [] = []
segment i xs = let (h, t) = splitAt i xs in h : segment i t
