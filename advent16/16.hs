module Main where

generateRandom :: Int -> String -> String
generateRandom len s | length s >= len = s
                     | otherwise       = generateRandom len $ step s
  where
    step :: String -> String
    step s = s ++ "0" ++ map swap (reverse s)

    swap :: Char -> Char
    swap '0' = '1'
    swap '1' = '0'
    swap c   = undefined

checksum :: String -> String
checksum s | odd (length s) = s
           | otherwise      = checksum $ genChecksum s
  where
    genChecksum :: String -> String
    genChecksum [] = []
    genChecksum s  = let (pair, rest) = splitAt 2 s
                         char = if head pair == last pair
                                  then '1' else '0'
                      in char : genChecksum rest

main :: IO ()
main = do
  indata <- getLine
  putStrLn . checksum . take 272 $ generateRandom 272 indata
