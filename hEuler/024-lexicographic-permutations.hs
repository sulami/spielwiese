-- Lexicographic permutations
-- Problem 24
--
-- A permutation is an ordered arrangement of objects. For example, 3124 is one
-- possible permutation of the digits 1, 2, 3 and 4. If all of the permutations
-- are listed numerically or alphabetically, we call it lexicographic order.
-- The lexicographic permutations of 0, 1 and 2 are:
--
-- 012   021   102   120   201   210
--
-- What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4,
-- 5, 6, 7, 8 and 9?

{-# OPTIONS_GHC -O2 #-}

import Data.List (sort)

main = putStrLn $ (sort $ map concat
  $ [ map show [a,b,c,d,e,f,g,h,i,j] |
  a <- [0..9],
  b <- [0..9],
  b /= a,
  c <- [0..9],
  c /= a, c /= b,
  d <- [0..9],
  d /= a, d /= b, d /= c,
  e <- [0..9],
  e /= a, e /= b, e /= c, e /= d,
  f <- [0..9],
  f /= a, f /= b, f /= c, f /= d, f /= e,
  g <- [0..9],
  g /= a, g /= b, g /= c, g /= d, g /= e, g /= f,
  h <- [0..9],
  h /= a, h /= b, h /= c, h /= d, h /= e, h /= f, h /= g,
  i <- [0..9],
  i /= a, i /= b, i /= c, i /= d, i /= e, i /= f, i /= g, i /= h,
  j <- [0..9],
  j /= a, j /= b, j /= c, j /= d, j /= e, j /= f, j /= g, j /= h, j /= i
  ]) !! 999999

