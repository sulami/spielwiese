module Main where

import Data.List (nub, permutations, sort, sortBy, subsequences)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.IO (hFlush, stdout)
import System.Random (randomRIO)

data GameState = GameState [(String, Bool)] Int

subwords :: [String] -> String -> [String]
subwords list base = sortBy (comparing length) $ sort $ nub
                     $ filter (`elem` list) $ filter (\w -> 3 <= length w)
                     $ concat $ map permutations $ subsequences base

guess :: GameState -> String -> GameState
guess (GameState g0 s0) w0
  | new       = GameState (map setNew g0) $ s0 + length w0
  | otherwise = GameState g0 s0
    where
      new :: Bool
      new = w0 `elem` (map fst g0) && not (fromJust (lookup w0 g0))

      setNew :: (String, Bool) -> (String, Bool)
      setNew (w,s) = (w, s || w0 == w)

wordToStr :: (String, Bool) -> String
wordToStr (w,s) | s         = "\x1b[32m" ++ w ++ "\x1b[39m"
                | otherwise = [ '_' | _ <- [1..(length w)] ]

prompt :: GameState -> String -> String
prompt (GameState g0 s0) ls =
  let d = show $ length $ filter snd g0
      ad = show $ length g0
  in concat [ "[", d, "/", ad, "|", show s0, "] ", ls, " > " ]

printState :: GameState -> IO ()
printState (GameState g0 s0) = putStrLn $ unwords $ map wordToStr g0

mainLoop :: String -> GameState -> IO ()
mainLoop w0 gs0@(GameState g0 s0) = if all snd g0
                                      then putStrLn "Fin!"
                                      else do clearScreen
                                              setCursorPosition 0 0
                                              printState gs0
                                              putStr $ prompt gs0 w0
                                              hFlush stdout
                                              g <- getLine
                                              let g1 = guess gs0 g
                                              mainLoop w0 g1

main = do wordlist <- fmap lines $ readFile "words"
          let initlist = filter (\w -> length w == 6) wordlist
          initn <- randomRIO (0, length initlist - 1)
          let w0 = initlist !! initn
          let words = subwords wordlist w0
          let s0 = zip words $ repeat False
          mainLoop (sort w0) $ GameState s0 0

