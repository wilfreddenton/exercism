module Series (largestProduct) where

import Data.Char (digitToInt, isDigit)

largestProduct :: Int -> String -> Maybe Integer
largestProduct 0 _ = Just 1
largestProduct _ [] = Nothing
largestProduct size digits
  | size > length digits || size < 0 = Nothing
  | not $ all isDigit digits = Nothing
  | otherwise = Just $ maximum $ products size digits

products :: Int -> String -> [Integer]
products size digits
  | length digits < size = []
  | otherwise = product (map (toInteger . digitToInt) $ take size digits) : products size (tail digits)
