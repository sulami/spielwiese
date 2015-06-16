-- 10001st prime
-- Problem 7
--
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
-- that the 6th prime is 13.
--
-- What is the 10001st prime number?

{-# OPTIONS_GHC -O2 #-}

isPrime :: Integer -> Bool
isPrime n = 2 == length (divisors n)
  where
    divisors :: Integer -> [Integer]
    divisors n = 1 : [ x | x <- [2..(n `div` 2)], n `mod` x == 0] ++ [n]

primes :: [Integer]
primes = 2 : 3 : [n | n <- [5,7..], isPrime n]

main = print $ primes !! 10001

