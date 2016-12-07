module Main where

import           Data.Text (Text, pack, split, strip, unpack)

data Command = LeftC Int
             | RightC Int
             deriving (Show)

readCommand :: String -> Command
readCommand ('L':n) = LeftC $ read n
readCommand ('R':n) = RightC $ read n
readCommand _       = undefined

data Direction = North | East | South | West deriving (Show)

turn :: Command -> Direction -> Direction
turn (LeftC _)  North = West
turn (RightC _) North = East
turn (LeftC _)  East  = North
turn (RightC _) East  = South
turn (LeftC _)  South = East
turn (RightC _) South = West
turn (LeftC _)  West  = South
turn (RightC _) West  = North

getSteps :: Command -> Int
getSteps (LeftC n)  = n
getSteps (RightC n) = n

type Coord = (Int, Int)

walk :: Command -> Direction -> Coord -> [Coord]
walk cmd dir (x,y) = case dir of
  North -> [(x, y + s) | s <- [1..getSteps cmd]]
  South -> [(x, y - s) | s <- [1..getSteps cmd]]
  East  -> [(x + s, y) | s <- [1..getSteps cmd]]
  West  -> [(x - s, y) | s <- [1..getSteps cmd]]

distance :: Coord -> Int
distance (x,y) = abs x + abs y

path :: [Command] -> [Coord]
path cmds = cd cmds North (0,0)
  where
    cd :: [Command] -> Direction -> Coord -> [Coord]
    cd []       _   pos = [pos]
    cd (c:cmds) dir pos = let newdir = turn c dir
                              steps = walk c newdir pos
                          in pos : init steps ++ cd cmds newdir (last steps)

firstDuplicate :: Eq a => [a] -> a
firstDuplicate (x:xs) | x `elem` xs = x
                      | otherwise   = firstDuplicate xs
firstDuplicate []     = undefined

main :: IO ()
main = do
  cmds <- map (readCommand . unpack . strip) . split (== ',') . pack <$>
          readFile "01.input"
  let way = path cmds
  print . distance $ last way
  print . distance . firstDuplicate $ way
