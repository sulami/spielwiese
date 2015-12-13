module Main where

import Control.Arrow (second)
import Data.List (maximumBy, minimumBy, partition, permutations)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

dist :: (Eq k, Num v) => k -> k -> [(k, [(k, v)])] -> v
dist a b l = let d0 = fromJust . lookup a . fromJust $ lookup b l
                 d1 = fromJust . lookup b . fromJust $ lookup a l
              in d0 + d1

pathLength :: (Eq k, Num v) => [k] -> [(k, [(k, v)])] -> v
pathLength p = pathLength' p 0
  where
    pathLength' :: (Eq k, Num v) => [k] -> v -> [(k, [(k, v)])] -> v
    pathLength' [x]    d l = d
    pathLength' (x:xs) d l = pathLength' xs (d + dist x (head xs) l) l

tsp :: (Eq k, Ord k, Real v) => [(k, [(k, v)])] -> [(v, [k])]
tsp l = let paths = map (\l -> l ++ [head l]) . permutations $ map fst l
            dists = map (`pathLength` l) paths
         in zip dists paths

readValues :: [String] -> [(String, [(String, Integer)])]
readValues []     = []
readValues (l:ls) = let ws = words l
                        a = head ws
                        b = init $ last ws
                        h = read $ ws !! 3
                        fh = if "lose" `elem` ws then (- h) else h
                    in (a, [(b, fh)]) : readValues ls

merge :: [(String, [(String, Integer)])] -> [(String, [(String, Integer)])]
merge []         = []
merge ((x,y):xs) = let (same,diff) = partition (\(a,_) -> x == a) xs
                    in (x, y ++ concatMap snd same) : merge diff

addMyself :: [(String, [(String, Integer)])] -> [(String, [(String, Integer)])]
addMyself l = ("I", map (\x -> (fst x, 0)) l) : map (second ((:) ("I", 0))) l

main = do
  indata <- merge . readValues . lines <$> readFile "13.input"
  print . maximumBy (comparing fst) $ tsp indata
  print . maximumBy (comparing fst) . tsp $ addMyself indata

