import System.Environment (getArgs)
import System.IO (hFlush, stdout)

import FuzzyFinder

readL :: String -> [String]
readL a = read $ "[" ++ a ++ "]"

main = do args <- getArgs
          if length args == 1 then do
            list <- fmap readL . readFile $ head args
            putStr "Entery query: "
            hFlush stdout
            query <- getLine
            mapM putStrLn $ fuzzyFinder query list
            return ()
          else
            putStrLn "Error"

