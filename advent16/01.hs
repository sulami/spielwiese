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

walk :: Command -> Direction -> Coord -> Coord
walk cmd dir (x,y) = case dir of
  North -> (x, y + getSteps cmd)
  South -> (x, y - getSteps cmd)
  East -> (x + getSteps cmd, y)
  West -> (x - getSteps cmd, y)

calculateDistance :: [Command] -> Int
calculateDistance cmds = cd cmds North (0,0)
  where
    cd :: [Command] -> Direction -> Coord -> Int
    cd []       _   (x,y) = abs x + abs y
    cd (c:cmds) dir pos   = let newdir = turn c dir
                            in cd cmds newdir (walk c newdir pos)


main :: IO ()
main = do
  cmds <- map (readCommand . unpack . strip) . split (== ',') . pack <$>
          readFile "01.input"
  print $ calculateDistance cmds
