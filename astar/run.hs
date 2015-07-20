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
                                  g !! y1 !! x1 == '-' || g !! y1 !! x1 == 'F'
                                  ]

parseDir :: Coord -> Coord -> String
parseDir (x0,y0) (x1,y1) | x0 > x1 = "W"
                         | x0 < x1 = "E"
                         | y0 > y1 = "N"
                         | y0 < y1 = "S"

parsePath :: Path -> String
parsePath p = concat $ map (\(a,b) -> parseDir a b) $ zip (init p) (tail p)

go :: Grid -> Coord -> Path -> [Path]
go grid pos path | grid !! (snd pos) !! (fst pos) == 'F' = [path]
                 | otherwise = let pssbl = [(x,y) | (x,y) <- possibleWays grid pos,
                                        not $ (x,y) `elem` path]
                    in concat [go grid newpos $ path ++ [newpos] | newpos <- pssbl]

shortest :: [Path] -> Path
shortest ps = snd $ minimum $ zip (map length ps) ps

printPath :: String -> IO ()
printPath []     = return ()
printPath (x:xs) = do putStr $ x : "\n"
                      printPath xs

main = do grid <- fmap lines getContents
          let start = findStart grid 0
          let paths = go grid start [start]
          let short = shortest paths
          printPath $ parsePath short

