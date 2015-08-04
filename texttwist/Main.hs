module Main where

import Data.Ord (comparing)
import Data.List (nub, permutations, sort, sortBy, subsequences)
import System.Random (randomRIO)
import System.IO (hFlush, stdout)

type GameState = [(String, Bool)]

subwords :: [String] -> String -> [String]
subwords list base = sortBy (comparing length) $ nub $ filter (`elem` list)
                     $ concat $ filter (\w -> 2 < length w) $ map permutations
                     $ subsequences base

guess :: GameState -> String -> GameState
guess s0 w0 | w0 `elem` (map fst s0) = map (\(w,s) -> (w,s || w0 == w)) s0
            | otherwise              = s0

printState :: GameState -> IO ()
printState s0 = mapM_ putStrLn $ map filtrate s0
  where
    filtrate (w,s) | s         = w
                   | otherwise = "[" ++ show (length w) ++ " letters]"

mainLoop :: String -> GameState -> IO ()
mainLoop init s0 = if fin s0
                     then putStrLn "Fin!"
                     else do printState s0
                             putStr $ init ++ "> "
                             hFlush stdout
                             g <- getLine
                             let s1 = guess s0 g
                             mainLoop init s1
  where
    fin :: GameState -> Bool
    fin = all snd

main = do wordlist <- fmap lines $ readFile "words"
          let initlist = filter (\w -> length w > 3 && length w < 7) wordlist
          initn <- randomRIO (0, length initlist - 1)
          let init = initlist !! initn
          let words = subwords wordlist init
          let s0 = zip words $ repeat False
          mainLoop (sort init) s0

