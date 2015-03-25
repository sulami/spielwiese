import Data.List (foldl')
sum' :: (Num a) => [a] -> a
sum' = foldl' (+) 0
evenSum :: Integral a => [a] -> a
evenSum = sum' . (filter even)
squareEvenSum = evenSum . (map(^2))

main = print (squareEvenSum [1..10])

