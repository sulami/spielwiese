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
  input <- filter (/= ',') . clean <$> getLine
  print . sum . map score . fst $ readGroup input

clean :: String -> String
clean []       = []
clean ('!':xs) = clean $ tail xs
clean ('<':xs) = clean $ dropGarbage xs
clean (x:xs)   = x : clean xs

dropGarbage :: String -> String
dropGarbage ('!':xs) = dropGarbage $ tail xs
dropGarbage ('>':xs) = xs
dropGarbage (_:xs)   = dropGarbage xs
dropGarbage []       = []
