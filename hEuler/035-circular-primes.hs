-- Circular primes
-- Problem 35
--
-- The number, 197, is called a circular prime because all rotations of the
-- digits: 197, 971, and 719, are themselves prime.
--
-- There are thirteen such primes below 100:
-- 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
--
-- How many circular primes are there below one million?

{-# OPTIONS_GHC -O2 #-}

isPrime :: Integer -> Bool
isPrime n = [] == divisors n
  where
    divisors :: Integer -> [Integer]
    divisors n = [ x | x <- [3,5..(squareRoot n)], n `mod` x == 0]
    squareRoot :: Integer -> Integer
    squareRoot = ceiling . sqrt . (fromIntegral :: Integer -> Double)

rotate :: [a] -> [[a]]
rotate (x:xs) = let l = xs ++ [x] in l : rotate l

circularPrime :: Integer -> Bool
circularPrime n = let s = show n
                      p = map read $ take (length s) $ rotate s
                  in all odd p && all isPrime p

main = print $ length $ 2 : [x | x <- [3,5..999999], isPrime x, circularPrime x]

