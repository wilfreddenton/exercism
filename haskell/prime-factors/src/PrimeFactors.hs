module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors i
  | i < 2 = []
  | otherwise = x : primeFactors y
  where (x,y) = factors i 2
        factors n m = if n `mod` m == 0 then (m, n `div` m) else factors n o
          where o = if m == 2 then 3 else m + 2
