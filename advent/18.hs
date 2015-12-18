module Main where

import Data.Array

data Board = B
  { width   :: !Int
  , height  :: !Int
  , content :: !(Array Int Bool)
  }

instance Show Board where
  show (B w h c) = unlines . map (map toChar) . toLines $ elems c
    where
      toLines [] = []
      toLines r  = take w r : toLines (drop w r)
      toChar c = if c then '#' else '.'

step :: Board -> Board
step b0 = B (width b0) (height b0) . listArray (0,width b0 * height b0 - 1) $
  map lives [ (x,y) | x <- [0..width b0 - 1],
                      y <- [0..height b0 - 1] ]
  where
    lives :: (Int, Int) -> Bool
    lives coords = let n = length . filter inspect $ surFields coords
                       t = inspect coords
                    in (not t && n == 3) || (t && n >= 2 && n <= 3)

    inspect :: (Int, Int) -> Bool
    inspect (x,y) = content b0 ! (width b0 * y + x)

    surFields :: (Int, Int) -> [(Int, Int)]
    surFields (x,y) = [ (a,b) | a <- [x-1..x+1], b <- [y-1..y+1],
                                (a,b) /= (x,y),
                                a >= 0, a < width b0,
                                b >= 0, b < height b0 ]

parseBoard :: [String] -> Board
parseBoard ls = let w = length $ head ls
                    h = length ls
                in B w h . listArray (0, w*h-1) $ concatMap (map (== '#')) ls

main = do
  board <- parseBoard . lines <$> readFile "18.input"
  print . length . filter id . elems . content $ iterate step board !! 100

