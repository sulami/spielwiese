module Main where

import Data.List (nub, sort)
import Control.Parallel.Strategies (parMap, rpar)
import GHC.Conc.Sync (par, pseq)

type Grid = [String]
type Coord = (Int, Int)
type Path = [Coord]

find :: Grid -> Char -> Int -> Coord
find (x:xs) c n | c `elem` x = (f' x c 0, n)
                | otherwise  = find xs c $ n+1
  where
    f' :: String -> Char -> Int -> Int
    f' (x:xs) c n | c == x    = n
                  | otherwise = f' xs c $ n+1

flood :: Grid -> Coord -> Coord -> [(Int, Path)]
flood grid fin pos = fl grid fin [(cost fin [pos], [pos])]
  where
    fl :: Grid -> Coord -> [(Int, Path)] -> [(Int, Path)]
    fl grid fin paths
      | any (\p -> last p == fin) paths = filter (\p -> last p == fin) paths
      | otherwise = let best = snd $ minimum $ zip (parMap rpar (cost fin) paths) paths
                        pb = addRoutes grid paths best
                    in fl grid fin $ filter (/= best) paths ++ pb

    addRoutes :: Grid -> [Path] -> Path -> [Path]
    addRoutes grid ps path = [path ++ [p] | p <- possibleWays grid ps $ last path]

    possibleWays :: Grid -> [Path] -> Coord -> Path
    possibleWays g ps (x,y) = [(x1,y1) | y1 <- [(y-1)..(y+1)],
                                         y1 >= 0,
                                         y1 < length g,
                                         x1 <- [(x-1)..(x+1)],
                                         x1 >= 0,
                                         x1 < length (g !! y1),
                                         x-x1 == 0 || y-y1 == 0,
                                         g !! y1 !! x1 /= 'X',
                                         not $ (x1,y1) `elem` (concat ps)
                                         ]

    cost :: Coord -> Path -> Int
    cost fin path = let l = last path in (length path - 1) + (dist l fin)

    dist :: Coord -> Coord -> Int
    dist (x0,y0) (x1,y1) = abs (x0-x1) + abs (y0-y1)

drawPath :: Grid -> Path -> Grid
drawPath = foldr (\(x,y) r -> replace r x y '*')

replace :: [[a]] -> Int -> Int -> a -> [[a]]
replace o x y c = let (rpre,rpost) = splitAt y o
                      row = head rpost
                      (cpre,cpost) = splitAt x row
                  in rpre ++ [cpre ++ [c] ++ tail cpost] ++ tail rpost

main = do grid <- fmap lines getContents
          let start = find grid 'S' 0
          let fin = find grid 'F' 0
          let path = head $ flood grid fin start
          mapM_ putStrLn $ drawPath grid path

