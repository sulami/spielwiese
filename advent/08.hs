module Main where

alphaNum :: String
alphaNum = ['0'..'9'] ++ ['a'..'z']

validTokens :: String
validTokens = alphaNum ++ ['\\','"']

memoryLength :: String -> Int
memoryLength [] = 0
memoryLength (x:xs)
  | x `elem` alphaNum = 1 + memoryLength xs
  | x == '"'          = memoryLength xs
  | x == '\\'         = if head xs `elem` ['\\','"']
                          then 1 + memoryLength (drop 1 xs)
                          else 1 + memoryLength (drop 3 xs)

encodedLength :: String -> Int
encodedLength [] = 0
encodedLength (x:xs)
  | x `elem` alphaNum           = 1 + encodedLength xs
  | x == '\\'                   = 2 + encodedLength xs
  | x == '"'                    = 2 + encodedLength xs

main = do
  indata <- readFile "08.input"
  let pd = filter (`elem` validTokens) indata
      cl = length pd
      ml = memoryLength pd
      nl = length $ lines indata
  print $ cl - ml
  print $ (nl * 2 + encodedLength pd) - cl

