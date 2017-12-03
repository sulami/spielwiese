#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc

module Main where

main :: IO ()
main = do
  input <- read <$> getLine :: IO Int
  print $ manhattan !! (input - 1)

manhattan :: [Int]
manhattan = 0 : concat (man' limits)
  where
    man' lims = let ([a,b], rest) = splitAt 2 lims
                in replicate 3 (subseq a) ++ [stepseq b] ++ man' rest

subseq :: (Int, Int) -> [Int]
subseq (low,high) = let sub = [low..high] in init sub ++ reverse (drop 1 sub)

stepseq :: (Int, Int) -> [Int]
stepseq (low,high) = let sub = [low..high] in init sub ++ reverse (drop 2 sub)

limits :: [(Int, Int)]
limits = (1, 2) : limits' (1,2)
  where
    limits' pair = let [step, sub] = next' pair in step : sub : limits' sub
    next' (x,y) = [(x, y + 1), (x + 1, y + 2)]
