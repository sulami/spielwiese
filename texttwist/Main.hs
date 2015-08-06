module Main where

import Data.Ord (comparing)
import Data.List (nub, permutations, sort, sortBy, subsequences)
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.IO (hFlush, stdout)
import System.Random (randomRIO)

data GameState = GameState [(String, Bool)] Integer

subwords :: [String] -> String -> [String]
subwords list base = sortBy (comparing length) $ sort $ nub
                     $ filter (`elem` list) $ concat
                     $ filter (\w -> 2 < length w) $ map permutations
                     $ subsequences base

guess :: GameState -> String -> GameState
guess (GameState g0 s0) w0
  | w0 `elem` (map fst g0) = GameState (map (\(w,s) -> (w,s || w0 == w)) g0) s0
  | otherwise              = GameState g0 s0

printState :: GameState -> IO ()
printState (GameState g0 s0) = putStrLn $ unwords $ map filtrate g0
  where
    filtrate (w,s) | s         = "\x1b[32m" ++ w ++ "\x1b[39m"
                   | otherwise = [ '_' | _ <- [1..(length w)] ]

mainLoop :: String -> GameState -> IO ()
mainLoop w0 gs0@(GameState g0 s0) = if all snd g0
                                      then putStrLn "Fin!"
                                      else do clearScreen
                                              setCursorPosition 0 0
                                              printState gs0
                                              putStr prompt
                                              hFlush stdout
                                              g <- getLine
                                              let g1 = guess gs0 g
                                              mainLoop w0 g1
  where
    prompt :: String
    prompt = let d = show $ length $ filter snd g0
              in "[" ++ d ++ "/" ++ show (length g0) ++ "] " ++ w0 ++ " > "

main = do wordlist <- fmap lines $ readFile "words"
          let initlist = filter (\w -> length w == 6) wordlist
          initn <- randomRIO (0, length initlist - 1)
          let w0 = initlist !! initn
          let words = subwords wordlist w0
          let s0 = zip words $ repeat False
          mainLoop (sort w0) $ GameState s0 0

