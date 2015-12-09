module Main where

import Data.List (maximumBy, minimumBy, permutations)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

distances :: [(String, [(String, Integer)])]
distances =
  [
    ("Faerun", [
      ("Tristram",       65),
      ("Tambi",         129),
      ("Norrath",       144),
      ("Snowdin",        71),
      ("Straylight",    137),
      ("AlphaCentauri",   3),
      ("Abre",          149)
    ]),
    ("Tristram", [
      ("Faerun",         65),
      ("Tambi",          63),
      ("Norrath",         4),
      ("Snowdin",       105),
      ("Straylight",    125),
      ("AlphaCentauri",  55),
      ("Abre",           14)
    ]),
    ("Tambi", [
      ("Faerun",        129),
      ("Tristram",       63),
      ("Norrath",        68),
      ("Snowdin",        52),
      ("Straylight",     65),
      ("AlphaCentauri",  22),
      ("Abre",          143)
    ]),
    ("Norrath", [
      ("Faerun",        144),
      ("Tristram",        4),
      ("Tambi",          68),
      ("Snowdin",         8),
      ("Straylight",     23),
      ("AlphaCentauri", 136),
      ("Abre",          115)
    ]),
    ("Snowdin", [
      ("Faerun",         71),
      ("Tristram",      105),
      ("Tambi",          52),
      ("Norrath",         8),
      ("Straylight",    101),
      ("AlphaCentauri",  84),
      ("Abre",           96)
    ]),
    ("Straylight", [
      ("Faerun",        137),
      ("Tristram",      125),
      ("Tambi",          65),
      ("Norrath",        23),
      ("Snowdin",       101),
      ("AlphaCentauri", 107),
      ("Abre",           14)
    ]),
    ("AlphaCentauri", [
      ("Faerun",          3),
      ("Tristram",       55),
      ("Tambi",          22),
      ("Norrath",       136),
      ("Snowdin",        84),
      ("Straylight",    107),
      ("Abre",           46)
    ]),
    ("Abre", [
      ("Faerun",        149),
      ("Tristram",       14),
      ("Tambi",         143),
      ("Norrath",       115),
      ("Snowdin",        96),
      ("Straylight",     14),
      ("AlphaCentauri",  46)
    ])
  ]

dist :: (Eq k, Num v) => k -> k -> [(k, [(k, v)])] -> Maybe v
dist a b = foldr (\(k, v) no -> if a == k then dist' b v else no) Nothing
  where
    dist' :: (Eq k, Num v) => k -> [(k, v)] -> Maybe v
    dist' b = foldr (\(k,v) no -> if b == k then Just v else no) Nothing

pathLength :: (Eq k, Num v) => [k] -> [(k, [(k, v)])] -> v
pathLength p = pathLength' p 0
  where
    pathLength' :: (Eq k, Num v) => [k] -> v -> [(k, [(k, v)])] -> v
    pathLength' [x]    d l = d
    pathLength' (x:xs) d l = pathLength' xs (d + fromJust (dist x (head xs) l)) l

tsp :: (Eq k, Ord k, Real v) => [(k, [(k, v)])] -> [(v, [k])]
tsp l = let paths = permutations $ map fst l
            dists = map (`pathLength` l) paths
         in zip dists paths

main = do
  print . minimumBy (comparing fst) $ tsp distances
  print . maximumBy (comparing fst) $ tsp distances

