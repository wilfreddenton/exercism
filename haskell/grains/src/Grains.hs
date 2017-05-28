module Grains (square, total) where

import Data.Maybe (fromJust)

square :: Integer -> Maybe Integer
square n = if n <= 64 && n > 0 then Just $ 2 ^ (n - 1) else Nothing

total :: Integer
total = sum $ map (fromJust . square) [1..64]
