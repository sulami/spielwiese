-- Longest Collatz sequence
-- Problem 14
--
-- The following iterative sequence is defined for the set of positive
-- integers:
--
-- n -> n/2 (n is even)
-- n -> 3n + 1 (n is odd)
--
-- Using the rule above and starting with 13, we generate the following
-- sequence:
-- 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
--
-- It can be seen that this sequence (starting at 13 and finishing at 1)
-- contains 10 terms. Although it has not been proved yet (Collatz Problem), it
-- is thought that all starting numbers finish at 1.
--
-- Which starting number, under one million, produces the longest chain?
--
-- NOTE: Once the chain starts the terms are allowed to go above one million.

{-# OPTIONS_GHC -O2 #-}

import qualified Data.List as L

collatzLength :: Integer -> Integer -> Integer
collatzLength c n | n == 1 = c + 1
                  | even n = collatzLength (c + 1) (n `div` 2)
                  |  odd n = collatzLength (c + 1) (3 * n + 1)

maxLength :: Integer -> Integer
maxLength n = snd $ last $ L.sort $ zip (map (collatzLength 0) [1..n]) [1..n]

main = print $ maxLength 1000000

