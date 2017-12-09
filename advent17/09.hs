#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc

module Main where

newtype Group = Element [Group]
  deriving Show

readGroup :: String -> ([Group], String)
readGroup ('{':xs) = let (this, inner) = readGroup xs
                         (remains, rest) = readGroup inner
                     in (Element this : remains, rest)
readGroup ('}':xs) = ([], xs)
readGroup []       = ([], "")
readGroup _        = error "not a group"

score :: Group -> Int
score = score' 0
  where
    score' :: Int -> Group -> Int
    score' n (Element []) = n + 1
    score' n (Element xs) = n + 1 + sum (map (score' (n + 1)) xs)

main :: IO ()
main = do
  input <- getLine
  let nonIgnored = stripIgnored input
      cleaned = filter (/= ',') $ clean nonIgnored
  print . sum . map score . fst $ readGroup cleaned
  print . length . concat $ garbage nonIgnored

stripIgnored :: String -> String
stripIgnored ""       = ""
stripIgnored ('!':xs) = stripIgnored $ tail xs
stripIgnored (x:xs)   = x : stripIgnored xs

clean :: String -> String
clean []       = []
clean ('!':xs) = clean $ tail xs
clean ('<':xs) = clean $ dropGarbage xs
clean (x:xs)   = x : clean xs

dropGarbage :: String -> String
dropGarbage ('>':xs) = xs
dropGarbage (_:xs)   = dropGarbage xs
dropGarbage []       = []

garbage :: String -> [String]
garbage ('<':xs) = let (this, rest) = span (/= '>') xs
                   in this : garbage rest
garbage (_:xs)   = garbage xs
garbage []       = []
