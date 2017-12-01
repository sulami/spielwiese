module Main where

memoryLength :: String -> Int
memoryLength [] = 0
memoryLength (x:xs)
  | x == '"'  = memoryLength xs
  | x == '\\' = if head xs `elem` ['\\','"']
                  then 1 + memoryLength (drop 1 xs)
                  else 1 + memoryLength (drop 3 xs)
  | otherwise = 1 + memoryLength xs

encodedLength :: String -> Int
encodedLength = foldr (\c r -> if c `elem` ['\\','"'] then r + 2 else r + 1) 0

main = do
  indata <- readFile "08.input"
  let pd = concat $ lines indata
      cl = length pd
      ml = memoryLength pd
      nl = length $ lines indata
  print $ cl - ml
  print $ (nl * 2 + encodedLength pd) - cl

