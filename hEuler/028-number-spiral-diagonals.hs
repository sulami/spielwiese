-- Number spiral diagonals
-- Problem 28
--
-- Starting with the number 1 and moving to the right in a clockwise direction
-- a 5 by 5 spiral is formed as follows:
--
-- 21 22 23 24 25
-- 20  7  8  9 10
-- 19  6  1  2 11
-- 18  5  4  3 12
-- 17 16 15 14 13
--
-- It can be verified that the sum of the numbers on the diagonals is 101.
--
-- What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral
-- formed in the same way?

{-# OPTIONS_GHC -O2 #-}

add4 :: [Integer] -> Integer -> [Integer]
add4 l s |  s < 1000 = add4 next4 $ s + 2
         | otherwise = next4
  where
    next4 = l ++ (map (\x -> last l + s*x) [1..4])

main = print $ sum $ add4 [1] 2

