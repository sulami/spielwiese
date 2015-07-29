module Main where

import Astar

possibleWays :: PossibleWaysFun
possibleWays g (x,y) = [ (x1,y1) | y1 <- [(y-1)..(y+1)],
                                   y1 >= 0,
                                   y1 < length g,
                                   x1 <- [(x-1)..(x+1)],
                                   x1 >= 0,
                                   x1 < length (g !! y1),
                                   x-x1 == 0 || y-y1 == 0,
                                   g !! y1 !! x1 /= 'X' ]

cost :: CostFun
cost fin path = let l = last path in (length path - 1) + (dist l fin)
  where
    dist :: Coord -> Coord -> Int
    dist (x0,y0) (x1,y1) = abs (x0-x1) + abs (y0-y1)

find :: Grid -> Char -> Int -> Coord
find (x:xs) c n | c `elem` x = (f' x c 0, n)
                | otherwise  = find xs c $ n+1
  where
    f' :: String -> Char -> Int -> Int
    f' (x:xs) c n | c == x    = n
                  | otherwise = f' xs c $ n+1

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
          let path = flood grid fin start possibleWays cost
          mapM_ putStrLn $ drawPath grid path

