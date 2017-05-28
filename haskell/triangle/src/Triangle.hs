module Triangle (TriangleType(..), triangleType) where

import Data.List (sort)

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Ord a, Num a) => a -> a -> a -> TriangleType
triangleType 0 0 0 = Illegal
triangleType a b c
  | isInvalid a b c = Illegal
  | a == b && b == c = Equilateral
  | a == b || a == c || b == c = Isosceles
  | otherwise = Scalene
  where isInvalid x y z = let sides = sort [x,y,z] in sum (init sides) < last sides
