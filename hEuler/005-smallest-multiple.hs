-- Smallest multiple
-- Problem 5
--
-- 2520 is the smallest number that can be divided by each of the numbers from
-- 1 to 10 without any remainder.
--
-- What is the smallest positive number that is evenly divisible by all of the
-- numbers from 1 to 20?

{-# OPTIONS_GHC -O2 #-}

check :: Int -> Bool
check n = all ((== 0) . (mod n)) [20,19..1]

main :: IO ()
main = print . head $ filter check [1..]
