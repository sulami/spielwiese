module Main where

import Astar

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
          let path = head $ flood grid fin start
          mapM_ putStrLn $ drawPath grid path

