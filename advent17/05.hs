#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc

module Main where

type ModifierF = Int -> Int
type Maze = [Int]
type Instruction = Int

main :: IO ()
main = do
  input <- map read . lines <$> getContents :: IO Maze
  print $ walk modify1 0 0 input
  print $ walk modify2 0 0 input

walk :: ModifierF -> Int -> Instruction -> Maze -> Int
walk modifier ctr pos maze
  | pos < 0 || pos >= length maze = ctr
  |                     otherwise = let (np, nm) = jump modifier pos maze
                                    in walk modifier (ctr + 1) np nm

jump :: ModifierF -> Instruction -> Maze -> (Instruction, Maze)
jump modifier pos maze = let npos = pos + maze !! pos
                             (h,t) = splitAt pos maze
                             nmaze = h ++ [modifier $ head t] ++ tail t
                          in (npos, nmaze)

modify1 :: ModifierF
modify1 = (+ 1)

modify2 :: ModifierF
modify2 n |    n >= 3 = n - 1
          | otherwise = n + 1
