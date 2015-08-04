module Main where

import Data.List (nub, permutations, subsequences)
import System.Random (randomRIO)

subwords :: [String] -> String -> [String]
subwords list base = nub $ filter (`elem` list) $ concat
                     $ filter (\w -> 2 < length w) $ map permutations
                     $ subsequences base

main = do wordlist <- fmap lines $ readFile "words"
          let initlist = filter (\w -> length w > 3 && length w < 7) wordlist
          initn <- randomRIO (0, length initlist - 1)
          let init = initlist !! initn
          putStrLn $ "Init: " ++ init
          mapM_ putStrLn $ subwords wordlist init

