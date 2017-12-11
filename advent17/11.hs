#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc

module Main where

data Direction = N | NE | SE | S | SW | NW
  deriving Show

type Position = (Int, Int)

readDirection :: String -> Direction
readDirection s = case s of
  "n"  -> N
  "ne" -> NE
  "se" -> SE
  "s"  -> S
  "sw" -> SW
  "nw" -> NW
  _    -> error "not a direction"

main :: IO ()
main = do
  input <- reverse . map readDirection . splitOn (== ',') <$> getLine
  let path = scanr move (0,0) input
  print . solve $ head path
  print . maximum $ map solve path

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f xs = let (this, rest) = break f xs
               in this : splitOn f (drop 1 rest)

move :: Direction -> Position -> Position
move N (x,y)  = (x,y+1)
move NE (x,y) = (x+1,y)
move SE (x,y) = (x+1,y-1)
move S (x,y)  = (x,y-1)
move SW (x,y) = (x-1, y)
move NW (x,y) = (x-1, y+1)

solve :: Position -> Int
solve (x,y) = abs $ maximum [x,y]
