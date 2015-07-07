-- Pandigital multiples
-- Problem 38
--
-- Take the number 192 and multiply it by each of 1, 2, and 3:
--
--   192 × 1 = 192
--   192 × 2 = 384
--   192 × 3 = 576
--
-- By concatenating each product we get the 1 to 9 pandigital, 192384576. We
-- will call 192384576 the concatenated product of 192 and (1,2,3)
--
-- The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4,
-- and 5, giving the pandigital, 918273645, which is the concatenated product
-- of 9 and (1,2,3,4,5).
--
-- What is the largest 1 to 9 pandigital 9-digit number that can be formed as
-- the concatenated product of an integer with (1,2, ... , n) where n > 1?

{-# OPTIONS_GHC -O2 #-}

import Data.List (sort)

pandigital :: Integer -> Bool
pandigital n = concat (map show [1..9]) == sort (show n)

muco :: Integer -> Integer -> Integer
muco n p = read $ concat $ map show $ map (*n) [1..p]

check :: Integer -> Integer -> Bool
check n p = pandigital $ muco n p

main = print $ maximum [muco n p | n <- [1..10000], p <- [1..2], check n p]

