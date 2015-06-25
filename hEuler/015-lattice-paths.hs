-- Lattice paths
-- Problem 15
--
-- Starting in the top left corner of a 2×2 grid, and only being able to move
-- to the right and down, there are exactly 6 routes to the bottom right
-- corner.
--
-- How many such routes are there through a 20×20 grid?

{-# OPTIONS_GHC -O2 #-}

paths :: Int -> Int
paths s = paths' s 0 0 0
  where
    paths' :: Int -> Int -> Int -> Int -> Int
    paths' s r x y | x < s && y < s = paths' s r (x+1) y + paths' s r x (y+1)
                   |          x < s = paths' s r (x+1) y
                   |          y < s = paths' s r x (y+1)
                   |      otherwise = r+1

main = print $ paths 20

