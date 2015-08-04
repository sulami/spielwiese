module Main where

import Data.Ord (comparing)
import Data.List (nub, permutations, sort, sortBy, subsequences)
import System.Random (randomRIO)

type GameState = [(String, Bool)]

subwords :: [String] -> String -> [String]
subwords list base = sortBy (comparing length) $ nub $ filter (`elem` list)
                     $ concat $ filter (\w -> 2 < length w) $ map permutations
                     $ subsequences base

printState :: GameState -> IO ()
printState s0 = mapM_ putStrLn $ map filtrate s0
  where
    filtrate (w,s) | s         = w
                   | otherwise = "[" ++ show (length w) ++ " letters]"

main = do wordlist <- fmap lines $ readFile "words"
          let initlist = filter (\w -> length w > 3 && length w < 7) wordlist
          initn <- randomRIO (0, length initlist - 1)
          let init = initlist !! initn
          let words = subwords wordlist init
          let s0 = zip words $ repeat False
          putStrLn $ "Letters: " ++ sort init
          printState s0

