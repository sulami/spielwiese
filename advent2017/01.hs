#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc

module Main where

main :: IO ()
main = do
  input <- getLine
  let result = sum . map (read . (:[])) $ matchesNext input :: Int
  print result

matchesNext :: String -> String
matchesNext xs = filter (/= ' ') $ zipWith sameOrBlank xs (tail $ cycle xs)
  where sameOrBlank a b |    a == b = a
                        | otherwise = ' '
