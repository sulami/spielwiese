-- Self powers
-- Problem 48
--
-- The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
--
-- Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

{-# OPTIONS_GHC -O2 #-}

digits :: Integral a => a -> [a]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

selfPower :: Integral a => a -> a
selfPower n = n^n

main = print $ mod (sum (map selfPower [1..1000])) (10^10)

