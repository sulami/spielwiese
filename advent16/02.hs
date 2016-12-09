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
keyNum (x, y) = 5 + x + 3 * y

keyNum2 :: Key -> Char
keyNum2 (_, -2) = '1'
keyNum2 (x, -1) = ['2'..'4'] !! (1 + x)
keyNum2 (x, 0)  = ['5'..'9'] !! (2 + x)
keyNum2 (x, 1)  = ['A'..'C'] !! (1 + x)
keyNum2 (_, 2)  = 'D'

within :: Ord a => (a, a) -> a -> a
within (lower, upper) = max lower . min upper

inBounds :: (Int, Int) -> (Int, Int) -> (Int, Int)
inBounds old new@(nx, ny) | abs nx + abs ny <= 2 = new
                          | otherwise            = old

move :: Key -> Direction -> Key
move (x, y) DUp    = (x, within (-1, 1) (y - 1))
move (x, y) DDown  = (x, within (-1, 1) (y + 1))
move (x, y) DLeft  = (within (-1, 1) (x - 1), y)
move (x, y) DRight = (within (-1, 1) (x + 1), y)

move2 :: Key -> Direction -> Key
move2 (x, y) DUp    = inBounds (x, y) (x, y - 1)
move2 (x, y) DDown  = inBounds (x, y) (x, y + 1)
move2 (x, y) DLeft  = inBounds (x, y) (x - 1, y)
move2 (x, y) DRight = inBounds (x, y) (x + 1, y)

type Command = [Direction]

execCommand :: Key -> Command -> Key
execCommand = foldl move

execCommand2 :: Key -> Command -> Key
execCommand2 = foldl move2

main :: IO ()
main = do
  cmds <- map (map readDirection) . lines <$> readFile "02.input"
  putStrLn . concatMap (show . keyNum) . tail $
    foldl' (\acc x -> acc ++ [execCommand (last acc) x]) [(0,0)] cmds
  putStrLn . map keyNum2 . tail $
    foldl' (\acc x -> acc ++ [execCommand2 (last acc) x]) [(0,0)] cmds
