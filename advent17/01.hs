#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc

module Main where

main :: IO ()
main = do
  input <- getLine
  print . sum . readDigits $ matchesNext input
  print . sum . readDigits $ matchesHalfway input

readDigits :: String -> [Int]
readDigits = map (read . (:[]))

matchesNext :: String -> String
matchesNext = matchesSomething (tail . cycle)

matchesHalfway :: String -> String
matchesHalfway = matchesSomething (\s -> drop (length s `div` 2) $ cycle s)

matchesSomething :: (String -> String) -> String -> String
matchesSomething trans xs = concat . zipWith sameOrEmpty xs $ trans xs
  where sameOrEmpty a b = if a == b then [a] else []
