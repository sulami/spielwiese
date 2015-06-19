maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
                [(x,"")] -> Just x
                _        -> Nothing

getListFromString :: String -> Maybe [Integer]
getListFromString s = maybeRead $ "[" ++ s ++ "]"

main = do putStrLn "Enter a list of comma-separated numbers:"
          input <- getLine
          let maybeList = getListFromString input in
            case maybeList of
              Just l  -> print (sum l)
              Nothing -> error "Bad format."

