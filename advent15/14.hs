module Main where

import Control.Arrow ((&&&))
import Data.List (maximumBy, nub)
import Data.Ord (comparing)

type Reindeer = (String, (Int, Int, Int))

reindeers :: [Reindeer]
reindeers = [
  ("Rudolph", (22,  8, 165)),
  ("Cupid",   ( 8, 17, 114)),
  ("Prancer", (18,  6, 103)),
  ("Donner",  (25,  6, 145)),
  ("Dasher",  (11, 12, 125)),
  ("Comet",   (21,  6, 121)),
  ("Blitzen", (18,  3,  50)),
  ("Vixen",   (20,  4,  75)),
  ("Dancer",  ( 7, 20, 119))
  ]

travelledAt :: Int -> Reindeer -> Int
travelledAt t r@(name, (v,vt,pt))
  | t > 0     = v * min t vt + travelledAt (t - vt - pt) r
  | otherwise = 0

speeds :: [Reindeer] -> [[(String, Int)]]
speeds rs = [ [ (fst &&& travelledAt t) r | r <- rs ] | t <- [1..2503] ]

leaders :: [(String, Int)] -> [String]
leaders rs = let best = maximum $ map snd rs
              in map fst $ filter (\(_,d) -> d == best) rs

points :: [String] -> [(String, Int)]
points rs = let names = nub rs
            in map (id &&& \n -> length $ filter (== n) rs) names

main = do
  let dists = map (fst &&& travelledAt 2503) reindeers
      winner = maximumBy (comparing snd)
  print $ winner dists
  print . winner . points . concatMap leaders $ speeds reindeers

