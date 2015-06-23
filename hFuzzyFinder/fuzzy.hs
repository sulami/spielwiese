-- This is a small and simple fuzzy filter that works by comparing a list of
-- chars, the search input, against a list of list of chars, the possible
-- solutions. If is case-insensitive and features scoring based on the length
-- of the matched pattern and the position of the matched pattern in the
-- result. There are no dependencies aside from the standard library. The usage
-- looks like this:
--
-- fuzzyFinder "ae" ["Alphabet", "Aneurysm", "Albatross"]
--
-- The code is licensed under ISC and
-- © Robin 'sulami' Schroer <sulami@peerwire.org>

module Sulami.FuzzyFinder (fuzzyFinder) where

import Data.Char
import Data.List

prepInput :: [[Char]] -> [([Char], [Char])]
prepInput i = zip i $ map (map toLower) i

partOf :: Char -> [Char] -> Int -> (Bool, Int)
partOf _ []     r = (False, 0)
partOf c (x:xs) r |    c == x = (True, r + 1)
                  | otherwise = partOf c xs $ r + 1

match :: [Char] -> ([Char], [Char]) -> (Bool, [Int])
match i s = match' i (snd s) []
  where
    match' :: [Char] -> [Char] -> [Int] -> (Bool, [Int])
    match' []     _ r = (True, r)
    match' (x:xs) s r | fst check = match' xs (drop (snd check) s)
                                           $ r ++ [snd check]
                      | otherwise = (False, r)
      where
        check = partOf x s 0

fuzzyFinder :: [Char] -> [[Char]] -> [[Char]]
fuzzyFinder i l = map fst . map snd $ c
  where
    c = sort $ zip (zip ((map sum . map tail) s) (map head s)) p
    s = map snd $ map (match i) p
    p = filter (fst . match i) $ prepInput l

