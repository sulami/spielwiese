module PartialFold where

foldP :: (a -> b -> Either b b) -> b -> [a] -> b
foldP _ acc []     = acc
foldP f acc (x:xs) = case f x acc of
                        (Right rv) -> rv
                        (Left nrv) -> foldP f nrv xs

toZero :: Int -> Int -> Either Int Int
toZero 0 c = Right c
toZero _ c = Left $ c + 1

