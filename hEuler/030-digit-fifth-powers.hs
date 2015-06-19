-- Digit fifth powers
-- Problem 30
--
-- Surprisingly there are only three numbers that can be written as the sum of
-- fourth powers of their digits:
--
-- 1634 = 1^4 + 6^4 + 3^4 + 4^4
-- 8208 = 8^4 + 2^4 + 0^4 + 8^4
-- 9474 = 9^4 + 4^4 + 7^4 + 4^4
--
-- As 1 = 14 is not a sum it is not included.
--
-- The sum of these numbers is 1634 + 8208 + 9474 = 19316.
--
-- Find the sum of all the numbers that can be written as the sum of fifth
-- powers of their digits.

{-# OPTIONS_GHC -O2 #-}

digits :: Integral a => a -> [a]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

check :: Integral a => a -> Bool
check n = n == foldl (\r e -> r + (e^5)) 0 (digits n)

-- sulami's note: 6*(9^5) ~= 350,000, so the biggest result will have six
-- digits at most.
main = print $ sum $ filter check [10..1000000]

