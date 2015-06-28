-- Pandigital prime
-- Problem 41
--
-- We shall say that an n-digit number is pandigital if it makes use of all the
-- digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is
-- also prime.
--
-- What is the largest n-digit pandigital prime that exists?

{-# OPTIONS_GHC -O2 #-}

import Data.List (sort)

isPrime :: Int -> Bool
isPrime n = divisors n == []
  where
    divisors :: Int -> [Int]
    divisors n = [ x | x <- [2..(n `div` 2)], n `mod` x == 0]

digits :: Integral a => a -> [a]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

pandigital :: Int -> Bool
pandigital n = sort (digits n) == [1..(length (show n))]

main = print $ last $ [x | x <- [1..987654321], pandigital x, isPrime x]

