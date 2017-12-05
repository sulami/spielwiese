#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc

module Main where

main :: IO ()
main = do
  input <- map read . lines <$> getContents :: IO [Int]
  print $ walk input

walk :: [Int] -> Int
walk = walk' 0 0
  where
    walk' :: Int -> Int -> [Int] -> Int
    walk' ctr pos maze
      | pos < 0 || pos >= length maze = ctr
      |                     otherwise = let (np, nm) = jump pos maze
                                        in walk' (ctr + 1) np nm

jump :: Int -> [Int] -> (Int, [Int])
jump pos maze = let npos = pos + maze !! pos
                    nmaze = updateMaze
                in (npos, nmaze)
  where
    updateMaze :: [Int]
    updateMaze = let (h,t) = splitAt pos maze
                  in h ++ [head t + 1] ++ tail t
