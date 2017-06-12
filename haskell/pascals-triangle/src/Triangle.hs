module Triangle (rows) where

rows :: Int -> [[Integer]]
rows x
  | x <= 0 = []
  | x == 1 = [[1]]
  | otherwise = prevRows ++ [1 : zipWith (+) prevRow (drop 1 prevRow) ++ [1]]
  where prevRows = rows $ x - 1
        prevRow = last prevRows
