module Main where

import Data.List (nub, permutations, sort, sortBy, subsequences)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.IO (hFlush, stdout)
import System.Random (randomRIO)

data GameState = GameState
  { solutions :: [(String, Bool)]
  , points    :: Int
  }

subwords :: [String] -> String -> [String]
subwords list base = sortBy (comparing length) . sort . nub
                     . filter (`elem` list) . filter (\w -> 3 <= length w)
                     . concatMap permutations $ subsequences base

guess :: GameState -> String -> GameState
guess (GameState g0 s0) w0
  | new       = GameState (map setNew g0) $ s0 + length w0
  | otherwise = GameState g0 s0
    where
      new :: Bool
      new = w0 `elem` map fst g0 && not (fromJust $ lookup w0 g0)

      setNew :: (String, Bool) -> (String, Bool)
      setNew (w,s) = (w, s || w0 == w)

wordToStr :: (String, Bool) -> String
wordToStr (w,s) | s         = "\x1b[32m" ++ w ++ "\x1b[39m"
                | otherwise = [ '_' | _ <- [1..(length w)] ]

prompt :: GameState -> String -> String
prompt (GameState ss ps) ls =
  let d = show . length $ filter snd ss
      ad = show $ length ss
  in concat [ "[", d, "/", ad, "|", show ps, "] ", ls, " > " ]

printState :: GameState -> IO ()
printState gs = putStrLn . unwords . map wordToStr $ solutions gs

mainLoop :: String -> GameState -> IO ()
mainLoop w gs = if all snd (solutions gs)
                  then putStrLn "Fin!"
                  else do
                    clearScreen
                    setCursorPosition 0 0
                    printState gs
                    putStr $ prompt gs w
                    hFlush stdout
                    g <- getLine
                    mainLoop w $ guess gs g

main = do wordlist <- lines <$> readFile "words"
          let initlist = filter (\w -> length w == 6) wordlist
          initn <- randomRIO (0, length initlist - 1)
          let w0 = initlist !! initn
          let words = subwords wordlist w0
          let s0 = zip words $ repeat False
          mainLoop (sort w0) $ GameState s0 0

