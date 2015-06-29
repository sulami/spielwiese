-- Eight Divisors
-- Problem 501
--
-- The eight divisors of 24 are 1, 2, 3, 4, 6, 8, 12 and 24.
-- The ten numbers not exceeding 100 having exactly eight divisors are 24, 30,
-- 40, 42, 54, 56, 66, 70, 78 and 88.
-- Let f(n) be the count of numbers not exceeding n with exactly eight
-- divisors.  You are given f(100) = 10, f(1000) = 180 and f(10^6) = 224427.
-- Find f(10^12).

{-# OPTIONS_GHC -O2 #-}

divisors :: Integer -> [Integer]
divisors n = [x | x <- [2..(n `div` 2)], n `mod` x == 0] ++ [n]

check :: Integer -> Bool
check n = length divs == 7 && last divs == n
  where
    divs = take 7 $ divisors n

main = print $ length [x | x <- [1..10^4], check x]

