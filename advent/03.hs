module Main where

import Prelude hiding (Either (..), traverse)

import Data.List (nub)

data Direction = Up
               | Down
               | Left
               | Right
               | Shite
               deriving (Eq, Show)

parseDirection :: Char -> Direction
parseDirection '^' = Up
parseDirection 'v' = Down
parseDirection '<' = Left
parseDirection '>' = Right
parseDirection  _  = Shite

type Coord = (Int, Int)

move :: Coord -> Direction -> Coord
move (x,y) Up    = (x+1,y)
move (x,y) Down  = (x-1,y)
move (x,y) Left  = (x,y-1)
move (x,y) Right = (x,y+1)
move (x,y) Shite = (x,y)

traverse :: [Coord] -> [Direction] -> [Coord]
traverse = foldl (\vis d -> move (head vis) d : vis)

divide :: ([a], [a]) -> [a] -> ([a], [a])
divide (a,b) []      = (a, b)
divide (a,b) [x]     = (a ++ [x], b)
divide (a,b) (x:y:z) = divide (a ++ [x], b ++ [y]) z

main = do
  indata <- map parseDirection <$> readFile "03"
  print . length . nub $ traverse [(0,0)] indata
  let (santa, robosanta) = divide ([],[]) indata
  print . length . nub $ traverse [(0,0)] santa ++ traverse [(0,0)] robosanta

