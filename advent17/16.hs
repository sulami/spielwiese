#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc

module Main where

import           Data.List  (elemIndex)
import           Data.Maybe (fromJust)

data DanceMove = Spin Int
               | Exchange Int Int
               | Partner Char Char
               deriving Show

readMove :: String -> DanceMove
readMove []     = error "not a dance move"
readMove (x:xs) = case x of
  's' -> Spin $ read xs
  'x' -> let [a,b] = splitOn (== '/') xs in Exchange (read a) (read b)
  'p' -> let [a,b] = splitOn (== '/') xs in Partner (head a) (head b)
  _   -> error "not a dance move"

type State = String

main :: IO ()
main = do
  input <- reverse . map readMove . splitOn (== ',') <$> getLine
  putStrLn $ foldr doMove ['a'..'p'] input

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f xs = let (this, rest) = break f xs
               in this : splitOn f (drop 1 rest)

doMove :: DanceMove -> State -> State
doMove (Spin n)       = spin n
doMove (Exchange a b) = exchange a b
doMove (Partner a b)  = partner a b

spin :: Int -> State -> State
spin = shift

exchange :: Int -> Int -> State -> State
exchange = swap

partner :: Char -> Char -> State -> State
partner a b xs = swap (fromJust $ elemIndex a xs) (fromJust $ elemIndex b xs) xs

shift :: Int -> [a] -> [a]
shift _ [] = []
shift 0 xs = xs
shift n xs = shift (n - 1) $ last xs : init xs

swap :: Int -> Int -> [a] -> [a]
swap _ _ [] = []
swap a b xs = [ xs !! translate i | i <- [0 .. length xs - 1] ]
  where translate n | n == a = b
                    | n == b = a
                    | otherwise = n
