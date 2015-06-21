-- Digit factorials
-- Problem 34
--
-- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
--
-- Find the sum of all numbers which are equal to the sum of the factorial
-- of their digits.
--
-- Note: as 1! = 1 and 2! = 2 are not sums they are not included.

{-# OPTIONS_GHC -O2 #-}

digits :: Integral a => a -> [a]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

factorial :: Integral a => a -> a
factorial n = foldl (\r e -> r * e) 1 [2..n]

curious :: Integer -> Bool
curious n = n == sum (map factorial (digits n))

-- sulami's note: 9! ~= 360,000, so the highest curious number will have 7
-- digits at most.
main = print $ sum $ filter curious [10..10000000]

