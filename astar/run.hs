module Main where

import           Data.List (sort, nub)

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


parsePath :: Path -> String
parsePath p = concat $ map (\(a,b) -> parseDir a b) $ zip (init p) (tail p)
  where
    parseDir :: Coord -> Coord -> String
    parseDir (x0,y0) (x1,y1) | x0 > x1 = "W"
                             | x0 < x1 = "E"
                             | y0 > y1 = "N"
                             | y0 < y1 = "S"

flood :: Grid -> Coord -> Coord -> [(Int, Path)]
flood grid fin pos = fl grid fin [(cost fin [pos], [pos])]
  where
    fl :: Grid -> Coord -> [(Int, Path)] -> [(Int, Path)]
    fl grid fin paths
      | any (\(_,p) -> last p == fin) paths = paths
      | otherwise = let cheap = sort $ paths
                        deadends = map fst $ takeWhile (\(_,r) -> null r) $ zip cheap $ map (addRoutes grid (map snd paths) . snd) cheap
                        ndes = map adjustCost deadends
                        nodeadends = drop (length ndes) cheap
                        best = head nodeadends
                        nr = addRoutes grid (map snd paths) $ snd best
                        pb = zip (map (cost fin) nr) nr
                    in fl grid fin $ pb ++ tail nodeadends ++ ndes
      where
        adjustCost :: (Int, Path) -> (Int, Path)
        adjustCost (c,p) = (c*10,p)

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
                                         not $ (x1,y1) `elem` (nub (concat ps))
                                         ]

    cost :: Coord -> Path -> Int
    cost fin path = let l = last path in (length path - 1) + (dist l fin)

    dist :: Coord -> Coord -> Int
    dist (x0,y0) (x1,y1) = abs (x0-x1) + abs (y0-y1)

printPath :: String -> IO ()
printPath []     = return ()
printPath (x:xs) = do putStrLn [x]
                      printPath xs

main = do grid <- fmap lines getContents
          let start = find grid 'S' 0
          let fin = find grid 'F' 0
          let path = snd $ head $ filter (\(_,p) -> last p == fin) $ flood grid fin start
          -- let paths = flood grid fin start
          -- print $ length paths
          printPath $ parsePath path

