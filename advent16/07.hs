module Main where

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
hasTLS (regs, hyps) = any hasABBA regs && all (not . hasABBA) hyps

main :: IO ()
main = do
  indata <- map readAddress . lines <$> readFile "07.input"
  print . length $ filter hasTLS indata
