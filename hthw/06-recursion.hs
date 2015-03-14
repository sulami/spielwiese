evenSum :: Integral a => [a] -> a

evenSum = accumSum 0
    where
        accumSum n [] = n
        accumSum n (x:xs) =
            if even x
                then accumSum (n+x) xs
                else accumSum n xs

