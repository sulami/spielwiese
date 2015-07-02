-- Powerful digit sum
-- Problem 56
--
-- A googol (10^100) is a massive number: one followed by one-hundred zeros;
-- 100100 is almost unimaginably large: one followed by two-hundred zeros.
-- Despite their size, the sum of the digits in each number is only 1.
--
-- Considering natural numbers of the form, ab, where a, b < 100, what is the
-- maximum digital sum?

{-# OPTIONS_GHC -O2 #-}

digits :: Integral a => a -> [a]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

main = print $ maximum [sum(digits(a^b)) | a <- [1..99], b <- [1..99]]

