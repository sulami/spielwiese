-- Smallest multiple
-- Problem 5
--
-- 2520 is the smallest number that can be divided by each of the numbers from
-- 1 to 10 without any remainder.
--
-- What is the smallest positive number that is evenly divisible by all of the
-- numbers from 1 to 20?

{-# OPTIONS_GHC -O2 #-}

check :: Integer -> Bool
check n = foldr (\e r -> if n `mod` e == 0 then r else False) True [1..20]

work :: Integer -> Integer
work n | check n   = n
       | otherwise = work (n+1)

main = print $ work 1

