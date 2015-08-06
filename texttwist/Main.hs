module Main where

import Data.Ord (comparing)
import Data.List (nub, permutations, sort, sortBy, subsequences)
import System.Console.ANSI (clearScreen)
import System.IO (hFlush, stdout)
import System.Random (randomRIO)

type GameState = [(String, Bool)]

subwords :: [String] -> String -> [String]
subwords list base = sortBy (comparing length) $ sort $ nub
                     $ filter (`elem` list) $ concat
                     $ filter (\w -> 2 < length w) $ map permutations
                     $ subsequences base

guess :: GameState -> String -> GameState
guess s0 w0 | w0 `elem` (map fst s0) = map (\(w,s) -> (w,s || w0 == w)) s0
            | otherwise              = s0

printState :: GameState -> IO ()
printState s0 = putStrLn $ unwords $ map filtrate s0
  where
    filtrate (w,s) | s         = "\x1b[32m" ++ w ++ "\x1b[39m"
                   | otherwise = [ '_' | _ <- [1..(length w)] ]

mainLoop :: String -> GameState -> IO ()
mainLoop w0 s0 = if all snd s0
                   then putStrLn "Fin!"
                   else do clearScreen
                           printState s0
                           putStr prompt
                           hFlush stdout
                           g <- getLine
                           let s1 = guess s0 g
                           mainLoop w0 s1
  where
    prompt :: String
    prompt = let d = show $ length $ filter snd s0
              in "[" ++ d ++ "/" ++ show (length s0) ++ "] " ++ w0 ++ " > "

main = do wordlist <- fmap lines $ readFile "words"
          let initlist = filter (\w -> length w == 6) wordlist
          initn <- randomRIO (0, length initlist - 1)
          let w0 = initlist !! initn
          let words = subwords wordlist w0
          let s0 = zip words $ repeat False
          mainLoop (sort w0) s0

