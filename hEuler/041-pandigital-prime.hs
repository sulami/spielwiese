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
isPrime n = 2 == length (divisors n)
  where
    divisors :: Int -> [Int]
    divisors n = 1 : [ x | x <- [2..(n `div` 2)], n `mod` x == 0] ++ [n]

primes :: [Int]
primes = 2 : 3 : [n | n <- [5,7..], isPrime n]

digits :: Integral a => a -> [a]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

pandigital :: Int -> Bool
pandigital n = sort (digits n) == [1..(length (digits n))]

main = print $ last $ takeWhile (<= 987654321) [x | x <- primes, pandigital x]

