module Main where

import Light

ls = LightSource (5,5) 7

asciiLight :: Int -> Char
asciiLight n = " .:-=+*#" !! (n `mod` 8)

printGrid :: [[Int]] -> IO ()
printGrid [] = return ()
printGrid (x:xs) = do putStrLn $ map asciiLight x
                      printGrid xs

main = do let grid = [ [ (x,y) | x <- [0..10] ] | y <- [0..10] ]
          let lgrid = map (map (`lightFromSource` ls)) grid
          printGrid lgrid

