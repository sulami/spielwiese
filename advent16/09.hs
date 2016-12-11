module Main where

import           Data.Char (isSpace)

decompress :: String -> String
decompress [] = []
decompress s  = let (native, compressed) = span (/= '(') s
                    (marker, afterwards) = span (/= ')') compressed
                    (count, multi) = span (/= 'x') $ tail marker
                    counter = read count :: Int
                    (base, rest) = splitAt counter $ tail afterwards
                    multiplier = read (tail multi) :: Int
                in if null compressed
                    then native
                    else native ++
                         take (counter * multiplier) (cycle base) ++
                         decompress rest

main :: IO ()
main = do
  indata <- readFile "09.input"
  print . length . filter (not . isSpace) $ decompress indata
