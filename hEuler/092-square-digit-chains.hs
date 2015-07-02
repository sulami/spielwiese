-- Square digit chains
-- Problem 92
--
-- A number chain is created by continuously adding the square of the digits in
-- a number to form a new number until it has been seen before.
--
-- For example,
--
-- 44 → 32 → 13 → 10 → 1 → 1
-- 85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89
--
-- Therefore any chain that arrives at 1 or 89 will become stuck in an endless
-- loop. What is most amazing is that EVERY starting number will eventually
-- arrive at 1 or 89.
--
-- How many starting numbers below ten million will arrive at 89?

{-# OPTIONS_GHC -O2 #-}

digits :: Integer -> [Integer]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

en :: Integer -> Bool
en 89 = True
en  1 = False
en  n = en $ sum . (map (^2)) . digits $ n

main = print $ length $ filter id $ map en [1..9999999]

