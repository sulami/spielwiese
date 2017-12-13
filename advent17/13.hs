#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc

module Main where

type Firewall = (Int, Int)

readFirewall :: String -> Firewall
readFirewall s = let [a,b] = map read . words $ filter (/= ':') s :: [Int] in (a, b)

severity :: Int -> Firewall -> Int
severity offset (d,r) = if caught offset (d,r) then d * r else 0

caught :: Int -> Firewall -> Bool
caught offset (d,r) = (offset + d) `mod` ((r - 1) * 2) == 0

main :: IO ()
main = do
  input <- map readFirewall . lines <$> getContents
  print . sum $ map (severity 0) input
  print . fst . head . filter snd $ map (\x -> (x, not (any (caught x) input))) [0..]
