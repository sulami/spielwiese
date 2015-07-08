-- Distinct primes factors
-- Problem 47
--
-- The first two consecutive numbers to have two distinct prime factors are:
--
--   14 = 2 × 7
--   15 = 3 × 5
--
-- The first three consecutive numbers to have three distinct prime factors
-- are:
--
--   644 = 2² × 7 × 23
--   645 = 3 × 5 × 43
--   646 = 2 × 17 × 19.
--
-- Find the first four consecutive integers to have four distinct prime
-- factors. What is the first of these numbers?

{-# OPTIONS_GHC -O2 #-}

divisors :: Integer -> [Integer]
divisors n = [x | x <- [2..(n `div` 2)], n `mod` x == 0]

isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n = [] == divisors n

primeFactors :: Integer -> [Integer]
primeFactors n = [x | x <- divisors n, isPrime x]

check :: Integer -> Bool
check n = let yep = (\x -> 4 == length (primeFactors x))
          in not $ all yep [n..(n+3)]

main = print $ head $ dropWhile check [1..]

