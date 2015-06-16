-- Summation of primes
-- Problem 10
--
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
--
-- Find the sum of all the primes below two million.

{-# OPTIONS_GHC -O2 #-}

isPrime :: Integer -> Bool
isPrime n = 2 == length (divisors n)
  where
    divisors :: Integer -> [Integer]
    divisors n = 1 : [ x | x <- [2..(n `div` 2)], n `mod` x == 0] ++ [n]

primes :: [Integer]
primes = 2 : 3 : [n | n <- [5,7..], isPrime n]

main = print $ sum $ takeWhile (< 2000000) primes

