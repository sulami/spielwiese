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

possibleWays :: Grid -> Coord -> Path
possibleWays g (x,y) = [(x1,y1) | x1 <- [0..(x+1)], y1 <- [0..(y+1)],
                                  abs (x-x1) + abs (y-y1) == 1,
                                  y1 < length g, x1 < length (g !! y1),
                                  g !! y1 !! x1 == 'F' || g !! y1 !! x1 == '-'
                                  ]

parseDir :: Coord -> Coord -> String
parseDir (x0,y0) (x1,y1) | x0 > x1 = "W"
                         | x0 < x1 = "E"
                         | y0 > y1 = "N"
                         | y0 < y1 = "S"

parsePath :: Path -> String
parsePath p = concat $ map (\(a,b) -> parseDir a b) $ zip (init p) (tail p)

flood :: Grid -> Coord -> [Path]
flood grid pos = fl grid [[pos]] [pos]
  where
    fl :: Grid -> [Path] -> [Coord] -> [Path]
    fl _ _ [] = []
    fl grid path pos
      | finished grid (last pos) = path
      | otherwise = let nexts = map (pssbl grid (last path)) pos
                    in path ++ concat (map (\n -> fl grid [last path ++ n] n) nexts)
    pssbl :: Grid -> Path -> Coord -> Path
    pssbl grid path pos = [(x,y) | (x,y) <- possibleWays grid pos,
                                   not $ (x,y) `elem` path]

finished :: Grid -> Coord -> Bool
finished g (x,y) = g !! y !! x == 'F'

cut :: Grid -> Path -> Path
cut _ [] = []
cut g p = takeWhile (not . finished g) p ++ (take 1 (dropWhile (not . finished g) p))

shortest :: [Path] -> Path
shortest ps = snd $ minimum $ zip (map length ps) ps

printPath :: String -> IO ()
printPath []     = return ()
printPath (x:xs) = do putStr $ x : "\n"
                      printPath xs

reachesTarget :: Grid -> Path -> Bool
reachesTarget g p = let (x,y) = last p in g !! y !! x == 'F'

main = do grid <- fmap lines getContents
          let start = findStart grid 0
          let paths = filter (reachesTarget grid)
                      $ map (cut grid) (flood grid start)
          let short = snd $ minimum $ zip (map length paths) paths
          printPath $ parsePath short

