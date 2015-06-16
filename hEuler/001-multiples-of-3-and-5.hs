-- Multiples of 3 and 5
-- Problem 1
--
-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we
-- get 3, 5, 6 and 9. The sum of these multiples is 23.
--
-- Find the sum of all the multiples of 3 or 5 below 1000.

{-# OPTIONS_GHC -O2 #-}

fb :: Int -> Bool
fb n | n `mod` 3 == 0 = True
     | n `mod` 5 == 0 = True
     |      otherwise = False

main = print $ sum $ filter fb [1..999]

