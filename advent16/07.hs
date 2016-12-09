module Main where

import           Data.List (isInfixOf)

type IPv7Address = ([String], [String])

readAddress :: String -> IPv7Address
readAddress = uninterlace . lines . map replaceBrackets
  where
    replaceBrackets :: Char -> Char
    replaceBrackets c = if c `elem` ['[', ']'] then '\n' else c

    uninterlace :: [a] -> ([a], [a])
    uninterlace []      = ([], [])
    uninterlace [x]     = ([x], [])
    uninterlace (x:y:z) = ([x], [y]) `mush` uninterlace z

    mush :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
    mush (a,b) (c,d) = (a ++ c, b ++ d)

isABBA :: String -> Bool
isABBA s = (head s /= s !! 1) && reverse (take 2 s) == drop 2 s

hasABBA :: String -> Bool
hasABBA s = length s >= 4 && (isABBA (take 4 s) || hasABBA (tail s))

hasTLS :: IPv7Address -> Bool
hasTLS (sups, hyps) = any hasABBA sups && all (not . hasABBA) hyps

palindrome :: String -> Bool
palindrome l = l == reverse l

findABAs :: String -> [String]
findABAs s
  | length s < 3                              = []
  | palindrome (take 3 s) && head s /= s !! 1 = take 3 s : findABAs (tail s)
  | otherwise                                 = findABAs (tail s)

hasSSL :: IPv7Address -> Bool
hasSSL (sups, hyps) = not $ null [bab | bab <- findBABs sups,
                                        hyp <- hyps,
                                        bab `isInfixOf` hyp]
  where
    findBABs :: [String] -> [String]
    findBABs = map invert . concatMap findABAs

    invert :: String -> String
    invert (a:b:_) = [b,a,b]

main :: IO ()
main = do
  indata <- map readAddress . lines <$> readFile "07.input"
  print . length $ filter hasTLS indata
  print . length $ filter hasSSL indata
