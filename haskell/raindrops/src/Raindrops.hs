module Raindrops (convert) where

divides :: Int -> Int -> Bool
divides x y = x `mod` y == 0

pling :: Int -> String
pling n
  | n `divides` 3 = "Pling"
  | otherwise = ""

plang :: Int -> String
plang n
  | n `divides` 5 = "Plang"
  | otherwise = ""

plong :: Int -> String
plong n
  | n `divides` 7 = "Plong"
  | otherwise =""

convert :: Int -> String
convert n
  | null rain = show n
  | otherwise = rain
  where rain = concatMap ($ n) [pling, plang, plong]
