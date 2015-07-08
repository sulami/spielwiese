module Sudoku (build, prettify, prettyPrint, solve) where

import Data.List (intersperse)

type Value = Int
type Coord = Int
type Cell  = (Coord, Value)
type Grid  = [Cell]

rowWise :: Coord -> [Coord]
rowWise n = let r = n `div` 9 * 9 in [r..(r+8)]

colWise :: Coord -> [Coord]
colWise n = let os = n`mod` 9 in [os,(os+9)..80]

boxWise :: Coord -> [Coord]
boxWise n = let c = n `mod` 9 `div` 3
                r = n `div` 27 * 3
                s = r * 9 + c * 3
             in [s..(s+2)] ++ [(s+9)..(s+11)] ++ [(s+18)..(s+20)]

get :: Grid -> Coord -> Value
get g n = snd $ g !! n

options :: Grid -> Cell -> [Value]
options g (n, _) = let r = (rowWise n) ++ (colWise n) ++ (boxWise n) in
                    filter (`notElem` (map (get g) r)) [1..9]

change :: Grid -> Cell -> Grid
change g (n, v) = map (\(gn, gv) -> if gn == n then (n, v) else (gn, gv)) g

fill :: Grid -> Cell -> Grid
fill g c@(n, v) | n >= 81          = g
                | v /= 0           = fill g next
                | length opts == 1 = fill (change g (n, (head opts))) next
                | otherwise        = fill g next
  where
    next = (n + 1, get g (n + 1))
    opts = options g c

solved :: Grid -> Bool
solved = foldr (\(_, v) r -> if v == 0 then False else r) True

solve :: Grid -> Grid
solve g | solved g  = g
        | otherwise = solve $ fill g (0, get g 0)

build :: [Value] -> Grid
build vs = zip [0..] vs

prettify :: Grid -> String
prettify g = unlines $ gaps $ fillempty $ pp (map snd g) []
  where
    pp [] r   = r
    pp g  r   = pp (drop 9 g) $ r ++ [cosho (take 9 g)]
    cosho     = concat . (map show)
    fillempty = map (map (\e -> if e == '0' then '_' else e))
    gaps      = map (intersperse ' ')

prettyPrint :: Grid -> IO ()
prettyPrint = putStr . prettify

