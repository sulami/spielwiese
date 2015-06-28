-- Summation of primes
-- Problem 10
--
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
--
-- Find the sum of all the primes below two million.

{-# OPTIONS_GHC -O2 #-}

isPrime :: Integer -> Bool
isPrime n = [] == [x | x <- [2..(n `div` 2)], n `mod` x == 0]

main = print $ sum $ [x | x <- [2..2000000], isPrime x]

