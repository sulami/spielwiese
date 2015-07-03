-- Number letter counts
-- Problem 17
--
-- If the numbers 1 to 5 are written out in words: one, two, three, four, five,
-- then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
--
-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out
-- in words, how many letters would be used?
--
-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
-- forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
-- letters. The use of "and" when writing out numbers is in compliance with
-- British usage.

{-# OPTIONS_GHC -O2 #-}

spoken :: Integer -> String
spoken n |  n <   20 = d1 n
         |  n <  100 = d2 (n `div` 10 * 10) ++ d1 (n `mod` 10)
         |  n < 1000 = d1 (n `div` 100) ++ d3 n
         | otherwise = "onethousand"
  where
    d1 0 = ""
    d1 1 = "one"
    d1 2 = "two"
    d1 3 = "three"
    d1 4 = "four"
    d1 5 = "five"
    d1 6 = "six"
    d1 7 = "seven"
    d1 8 = "eight"
    d1 9 = "nine"
    d1 10 = "ten"
    d1 11 = "eleven"
    d1 12 = "twelve"
    d1 13 = "thirteen"
    d1 14 = "fourteen"
    d1 15 = "fifteen"
    d1 16 = "sixteen"
    d1 17 = "seventeen"
    d1 18 = "eighteen"
    d1 19 = "nineteen"
    d2 20 = "twenty"
    d2 30 = "thirty"
    d2 40 = "forty"
    d2 50 = "fifty"
    d2 60 = "sixty"
    d2 70 = "seventy"
    d2 80 = "eighty"
    d2 90 = "ninety"
    d3 n  | 0 == n `mod` 100 = "hundred"
          |        otherwise = "hundredand" ++ spoken (n `mod` 100)

main = print $ length . concat $ map spoken [1..1000]

