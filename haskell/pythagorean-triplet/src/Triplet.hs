module Triplet (isPythagorean, mkTriplet, pythagoreanTriplets) where

import Data.List (permutations, permutations)

isPythagorean :: (Int, Int, Int) -> Bool
isPythagorean (a,b,c) = foldl (\v [x,y,z] -> (v || x^2 + y^2 == z^2)) False $ permutations [a,b,c]

mkTriplet :: Int -> Int -> Int -> (Int, Int, Int)
mkTriplet a b c = (a,b,c)

pythagoreanTriplets :: Int -> Int -> [(Int, Int, Int)]
pythagoreanTriplets minFactor maxFactor = filter isPythagorean combinations
  where range = [minFactor..maxFactor]
        combinations = [(a,b,c) | a <- range, b <- range, c <- range, a < b, b < c]
