module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = foldl (\acc val -> acc + zeroOrVal factors val) 0 [1..limit - 1]

zeroOrVal :: [Integer] -> Integer -> Integer
zeroOrVal [] _ = 0
zeroOrVal factors i = if isMultiple factors i False then i else 0

isMultiple :: [Integer] -> Integer -> Bool -> Bool
isMultiple [] _ _ = False
isMultiple (x:xs) i b = i `mod` x == 0 || isMultiple xs i b
