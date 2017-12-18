#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc

module Main where

main :: IO ()
main = do
  input <- read <$> getLine
  print . (\(_,p,s) -> s !! (p + 1)) . (!! 2017) $ iterate (fill input) (1,0,[0])

fill :: Int -> (Int, Int, [Int]) -> (Int, Int, [Int])
fill step (x,pos,s0) = let newPos = if null s0 then 0 else (pos + step) `mod` length s0 + 1
                           (h,t) = splitAt newPos s0
                       in (x + 1, newPos, h ++ [x] ++ t)
