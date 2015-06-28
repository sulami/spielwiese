-- Permuted multiples
-- Problem 52
--
-- It can be seen that the number, 125874, and its double, 251748, contain
-- exactly the same digits, but in a different order.
--
-- Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x,
-- contain the same digits.

{-# OPTIONS_GHC -O2 #-}

import Data.List (sort)

digits :: Integral a => a -> [a]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

check n | orig /= sort (digits(2*n)) = False
        | orig /= sort (digits(3*n)) = False
        | orig /= sort (digits(4*n)) = False
        | orig /= sort (digits(5*n)) = False
        | orig /= sort (digits(6*n)) = False
        |                  otherwise = True
  where
    orig = sort $ digits n

main = print $ head [x | x <- [1..], check x]

