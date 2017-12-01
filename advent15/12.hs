module Main where

validToken :: String
validToken =  "-0123456789"

filterNumbers :: String -> [String]
filterNumbers [] = []
filterNumbers l  = let d = dropWhile (not . (`elem` validToken)) l
                       (n,r) = span (`elem` validToken) d
                    in n : filterNumbers r

main = do
  indata <- readFile "12.input"
  print . sum . map read . filter (not . null) $ filterNumbers indata

