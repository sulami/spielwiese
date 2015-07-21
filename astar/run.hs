module Main where

type Grid = [String]
type Coord = (Int, Int)
type Path = [Coord]

findStart :: Grid -> Int -> Coord
findStart (x:xs) n | 'S' `elem` x = (fs x 0, n)
                   | otherwise    = findStart xs $ n+1
  where
    fs :: String -> Int -> Int
    fs (x:xs) n | 'S' == x  = n
                | otherwise = fs xs $ n+1


parsePath :: Path -> String
parsePath p = concat $ map (\(a,b) -> parseDir a b) $ zip (init p) (tail p)
  where
    parseDir :: Coord -> Coord -> String
    parseDir (x0,y0) (x1,y1) | x0 > x1 = "W"
                             | x0 < x1 = "E"
                             | y0 > y1 = "N"
                             | y0 < y1 = "S"

flood :: Grid -> Coord -> [Path]
flood grid pos = fl grid [[pos]]
  where
    fl :: Grid -> [Path] -> [Path]
    fl grid paths = paths ++ fl grid (concat (map (addRoutes grid paths) paths))

    addRoutes :: Grid -> [Path] -> Path -> [Path]
    addRoutes grid ps path = [ path ++ [p] | p <- possibleWays grid ps $ last path ]

    possibleWays :: Grid -> [Path] -> Coord -> Path
    possibleWays g ps (x,y) = [(x1,y1) | y1 <- [(y-1)..(y+1)],
                                         y1 >= 0,
                                         y1 < length g,
                                         x1 <- [(x-1)..(x+1)],
                                         x1 >= 0,
                                         x1 < length (g !! y1),
                                         abs (x-x1) + abs (y-y1) == 1,
                                         g !! y1 !! x1 /= 'X',
                                         not $ (x1,y1) `elem` (concat ps)
                                         ]

reachesTarget :: Grid -> Path -> Bool
reachesTarget g p = let (x,y) = last p in g !! y !! x == 'F'

printPath :: String -> IO ()
printPath []     = return ()
printPath (x:xs) = do putStr $ x : "\n"
                      printPath xs

main = do grid <- fmap lines getContents
          let start = findStart grid 0
          let path = head $ filter (reachesTarget grid) $ flood grid start
          printPath $ parsePath path

