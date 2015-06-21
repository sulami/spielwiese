-- Special Pythagorean triplet
-- Problem 9
--
-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for
-- which,
-- a^2 + b^2 = c^2
--
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
--
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

{-# OPTIONS_GHC -O2 #-}

main = print $ head [a*b*c | a <- [1..998],
                             b <- [2..999],
                             c <- [3..1000],
                             a < b, b < c,
                             a^2 + b^2 == c^2,
                             a + b + c == 1000]

