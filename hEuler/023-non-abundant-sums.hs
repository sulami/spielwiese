-- Non-abundant sums
-- Problem 23
--
-- A perfect number is a number for which the sum of its proper divisors is
-- exactly equal to the number. For example, the sum of the proper divisors of
-- 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect
-- number.
--
-- A number n is called deficient if the sum of its proper divisors is less
-- than n and it is called abundant if this sum exceeds n.
--
-- As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
-- number that can be written as the sum of two abundant numbers is 24. By
-- mathematical analysis, it can be shown that all integers greater than 28123
-- can be written as the sum of two abundant numbers. However, this upper limit
-- cannot be reduced any further by analysis even though it is known that the
-- greatest number that cannot be expressed as the sum of two abundant numbers
-- is less than this limit.
--
-- Find the sum of all the positive integers which cannot be written as the sum
-- of two abundant numbers.

{-# OPTIONS_GHC -O2 #-}

divisors :: Integral a => a -> [a]
divisors n = 1 : [ x | x <- [2..(n `div` 2)], n `mod` x == 0]

abundant :: Integral a => a -> Bool
abundant n | n < sum (divisors n) = True
           |            otherwise = False

abundantSum :: Integral a => a -> Bool
abundantSum n = foldl (\r e -> if abundant (n - e) then True else r)
                      False [x | x <- [12..(n `div` 2)], abundant x]

main = print $ sum $ filter abundantSum [1..28123]

