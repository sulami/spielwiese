toList :: String -> [Integer]
toList s = read ("[" ++ s  ++ "]")

main = do putStrLn "Enter a list of comma-separated numbers:"
          input <- getLine
          print $ sum $ toList input

