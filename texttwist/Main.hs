module Main where

import Data.List (nub, permutations, subsequences)

subwords :: [String] -> String -> [String]
subwords list base = nub $ filter (`elem` list) $ concat
                     $ filter (\w -> 2 < length w) $ map permutations
                     $ subsequences base

main = do wordlist <- fmap lines $ readFile "words"
          mapM_ putStrLn $ subwords wordlist "super"

