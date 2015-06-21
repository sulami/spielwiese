-- Largest palindrome product
-- Problem 4
--
-- A palindromic number reads the same both ways. The largest palindrome
-- made from the product of two 2-digit numbers is 9009 = 91 × 99.
--
-- Find the largest palindrome made from the product of two 3-digit numbers.

{-# OPTIONS_GHC -O2 #-}

palindromic :: Integer -> Bool
palindromic n = show n == reverse (show n)

main = print $ last [x*y | x <- [100..999],
                           y <- [100..999],
                           palindromic (x*y)]

