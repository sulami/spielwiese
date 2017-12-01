module Main where

las :: String -> String
las []     = []
las (x:xs) = let (same,rest) = span (== x) xs
              in show (1 + length same) ++ [x] ++ las rest

main = do
  let seq = iterate las "1113222113"
  print . length $ seq !! 40
  print . length $ seq !! 50

