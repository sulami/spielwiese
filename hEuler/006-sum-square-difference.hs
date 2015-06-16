-- Sum square difference
-- Problem 6
--
-- The sum of the squares of the first ten natural numbers is,
-- 12 + 22 + ... + 102 = 385
--
-- The square of the sum of the first ten natural numbers is,
-- (1 + 2 + ... + 10)^2 = 552 = 3025
--
-- Hence the difference between the sum of the squares of the first ten natural
-- numbers and the square of the sum is 3025 − 385 = 2640.
--
-- Find the difference between the sum of the squares of the first one hundred
-- natural numbers and the square of the sum.

squareSum :: [Integer] -> Integer
squareSum n = sum $ map (^2) n

sumSquare :: [Integer] -> Integer
sumSquare n = (sum n) ^ 2

diff :: [Integer] -> Integer
diff n = abs $ squareSum n - sumSquare n

main = print $ diff [1..100]

