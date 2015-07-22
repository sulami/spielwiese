module Main where

import Light

asciiLight :: Int -> Char
asciiLight n = " .:-=+*#" !! (n `mod` 8)

printGrid :: Grid -> IO ()
printGrid [] = return ()
printGrid (x:xs) = do putStrLn $ map (asciiLight . snd) x
                      printGrid xs

main = do let grid = [ [ ((x,y),0) | x <- [0..10] ] | y <- [0..10] ]
          let ls0 = LightSource ((3, 3), 0) 6
          let ls1 = LightSource ((7, 7), 0) 3
          let lgrid = lightInGrid grid [ls0, ls1]
          printGrid lgrid

