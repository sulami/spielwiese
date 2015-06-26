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

unique :: [Int] -> Bool
unique l = [0..2] == sort l

pretty :: [Int] -> String
pretty = foldr (\e r -> r ++ [head (show e)]) []

main = putStrLn . pretty . last $
        take (10^6) [ [a,b,c,d,e,f,g,h,i,j] | a <- [0..9],
                                              b <- [0..9],
                                              c <- [0..9],
                                              d <- [0..9],
                                              e <- [0..9],
                                              f <- [0..9],
                                              g <- [0..9],
                                              h <- [0..9],
                                              i <- [0..9],
                                              j <- [0..9],
                                              unique [a,b,c,d,e,f,g,h,i,j] ]

