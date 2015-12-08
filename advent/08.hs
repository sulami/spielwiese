module Main where

alphaNum :: String
alphaNum = ['0'..'9'] ++ ['a'..'z']

validTokens :: String
validTokens = alphaNum ++ ['\\','"']

codeLength :: String -> Int
codeLength = length . filter (`elem` validTokens)

memoryLength :: String -> Int
memoryLength [] = 0
memoryLength (x:xs)
  | x `elem` alphaNum = 1 + memoryLength xs
  | x == '"'          = memoryLength xs
  | x == '\\'         = if head xs `elem` ['\\','"']
                          then 1 + memoryLength (drop 1 xs)
                          else 1 + memoryLength (drop 3 xs)

main = do
  indata <- readFile "08.input"
  let cl = codeLength indata
      ml = memoryLength $ filter (`elem` validTokens) indata
  print $ cl - ml

