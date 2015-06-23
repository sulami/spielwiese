import System.Environment

import FuzzyFinder

readL :: String -> [String]
readL a = read $ "[" ++ a ++ "]"

main = do args <- getArgs
          if length args == 2 then do
            list <- readFile $ args !! 1
            print $ fuzzyFinder (args !! 0) $ readL list
          else
            print "Wrong number of args"
