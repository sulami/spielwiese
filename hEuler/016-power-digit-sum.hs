-- Power digit sum
-- Problem 16
--
-- 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
--
-- What is the sum of the digits of the number 2^1000?

{-# OPTIONS_GHC -O2 #-}

digits :: Integral a => a -> [a]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

main = print $ sum $ digits (2^1000)

