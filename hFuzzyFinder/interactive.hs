import System.Environment (getArgs)
import System.IO (hFlush, stdout)

import FuzzyFinder

readL :: String -> [String]
readL a = read $ "[" ++ a ++ "]"

putL :: [String] -> IO ()
putL [x]    = putStrLn x
putL (x:xs) = do putStrLn x
                 putL xs

main = do args <- getArgs
          if length args == 1 then do
            list <- fmap readL . readFile $ head args
            putStr "Entery query: "
            hFlush stdout
            query <- getLine
            putL $ fuzzyFinder query list
          else
            putStrLn "Wrong number of args"

