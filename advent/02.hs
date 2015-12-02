module Main where

import Data.List (sort)
import Data.Text (pack, split, unpack)

pkgSize :: String -> Int
pkgSize s = let td = split (== 'x') $ pack s
                [l,w,h] = map (read . unpack) td :: [Int]
            in (2*l*w) + (2*w*h) + (2*h*l) + minimum [l*w,w*h,h*l]

ribbonSize :: String -> Int
ribbonSize s = let td = split (== 'x') $ pack s
                   dim = map (read . unpack) td :: [Int]
                   sds = take 2 $ sort dim
                in product dim + sum (map (*2) sds)

main = do
  indata <- lines <$> readFile "02"
  print . sum $ map pkgSize indata
  print . sum $ map ribbonSize indata

