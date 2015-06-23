-- This is a small and simple fuzzy filter that works by comparing a list of
-- chars, the search input, against a list of list of chars, the possible
-- solutions. There are a couple of features still missing, like any form of
-- ranking or proper support for capital letters.
--
-- The code is licensed under ISC and
-- © Robin 'sulami' Schroer <sulami@peerwire.org>

module Sulami.FuzzyFinder (fuzzyFinder) where

import Data.Char

prepInput :: [[Char]] -> [[Char]]
prepInput = map (map toLower)

partOf :: Char -> [Char] -> (Bool, [Char])
partOf _ []     = (False, [])
partOf c (x:xs) |    c == x = (True, xs)
                | otherwise = partOf c xs

match :: [Char] -> [Char] -> Bool
match i s = match' i s
  where
    match' :: [Char] -> [Char] -> Bool
    match' []     _ = True
    match' (x:xs) s | fst check = match' xs (snd check)
                    | otherwise = False
      where
        check = partOf x s

fuzzyFinder :: [Char] -> [[Char]] -> [[Char]]
fuzzyFinder i l = filter (match i) $ prepInput l

