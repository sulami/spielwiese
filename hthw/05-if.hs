-- absolute :: (Ord a, Num a) => a -> a
absolute x = if x >= 0 then x else -x

main = print (absolute (-5))

