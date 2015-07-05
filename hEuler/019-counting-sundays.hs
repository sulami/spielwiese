-- Counting Sundays
-- Problem 19
--
-- You are given the following information, but you may prefer to do some
-- research for yourself.
--
--   1 Jan 1900 was a Monday.
--   Thirty days has September,
--   April, June and November.
--   All the rest have thirty-one,
--   Saving February alone,
--   Which has twenty-eight, rain or shine.
--   And on leap years, twenty-nine.
--   A leap year occurs on any year evenly divisible by 4, but not on a century
--   unless it is divisible by 400.
--
-- How many Sundays fell on the first of the month during the twentieth century
-- (1 Jan 1901 to 31 Dec 2000)?

{-# OPTIONS_GHC -O2 #-}

step :: (Int, Int, Int, Int, Int) -> Int
step (_, 31, 12, 2000, c) = c
step (w,  d,  m,    y, c) = step (nw, nd, nm, ny, nc)
  where
    nw = if w == 7 then 1 else w + 1
    nd | d == 28 && m == 2 && y `mod` 400 == 0                     = 29
       | d == 28 && m == 2 && y `mod`   4 == 0 && y `mod` 100 /= 0 = 29
       | d == 28 && m == 2 && y `mod`   4 /= 0                     =  1
       | d == 29 && m == 2                                         =  1
       | d == 31 && m == 12                                        =  1
       | d == 30 && m `elem` [4,6,9,11]                            =  1
       | d == 31 && m `elem` [1,2,3,5,7,8,10,12]                   =  1
       | otherwise                                                 =  d + 1
    nm | d == 28 && m == 2 && y `mod` 400 == 0                     = m
       | d == 28 && m == 2 && y `mod`   4 == 0 && y `mod` 100 /= 0 = m
       | d == 28 && m == 2 && y `mod`   4 /= 0                     = m + 1
       | d == 29 && m == 2                                         = m + 1
       | d == 31 && m == 12                                        = 1
       | d == 30 && m `elem` [4,6,9,11]                            = m + 1
       | d == 31 && m `elem` [1,2,3,5,7,8,10,12]                   = m + 1
       | otherwise                                                 = m
    ny | d == 31 && m == 12                                        = y + 1
       | otherwise                                                 = y
    nc | w == 7 && d == 1                                          = c + 1
       | otherwise                                                 = c

main = print $ step (2, 1, 1, 1901, 0)

