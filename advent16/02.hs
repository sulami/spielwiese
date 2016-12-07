module Main where

import           Data.List (foldl')

data Direction = DUp | DDown | DLeft | DRight deriving (Show)

readDirection :: Char -> Direction
readDirection 'U' = DUp
readDirection 'D' = DDown
readDirection 'L' = DLeft
readDirection 'R' = DRight

type Key = (Int, Int)

keyNum :: Key -> Int
keyNum (x, y) = 1 + x + (y * 3)

within :: Ord a => (a, a) -> a -> a
within (lower, upper) = max lower . min upper

move :: Key -> Direction -> Key
move (x, y) DUp    = (x, within (0, 2) (y - 1))
move (x, y) DDown  = (x, within (0, 2) (y + 1))
move (x, y) DLeft  = (within (0, 2) (x - 1), y)
move (x, y) DRight = (within (0, 2) (x + 1), y)

type Command = [Direction]

execCommand :: Key -> Command -> Key
execCommand = foldl move

main :: IO ()
main = do
  cmds <- map (map readDirection) . lines <$> readFile "02.input"
  putStrLn . concatMap (show . keyNum) . tail $
    foldl' (\acc x -> acc ++ [execCommand (last acc) x]) [(1,1)] cmds
