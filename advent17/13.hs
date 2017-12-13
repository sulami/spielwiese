#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc

module Main where

type Firewall = (Int, Int)

readFirewall :: String -> Firewall
readFirewall s = let [a,b] = map read . words $ filter (/= ':') s :: [Int] in (a, b)

severity :: Firewall -> Int
severity (d,r) = if d `mod` ((r - 1) * 2) == 0 then d * r else 0

main :: IO ()
main = do
  input <- map readFirewall . lines <$> getContents
  print . sum $ map severity input
