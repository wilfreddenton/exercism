module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import Control.Arrow ((&&&))
import Data.Vector (Vector, backpermute, enumFromStepN, toList, empty)
import qualified Data.Vector as V

data Matrix a = Matrix { cells :: Vector a
                       , rows :: Int
                       , cols :: Int }
                       deriving (Eq, Show)

column :: Int -> Matrix a -> Vector a
column x (Matrix cs h w) = backpermute cs $ enumFromStepN x w h

flatten :: Matrix a -> Vector a
flatten = cells

fromList :: [[a]] -> Matrix a
fromList [] = Matrix empty 0 0
fromList xss = Matrix (V.fromList $ concat xss) (length xss) (length $ head xss)

fromString :: Read a => String -> Matrix a
fromString = fromList . map parse . lines
  where parse s = case reads s of
                    [(x,xs)] -> x:parse xs
                    _ -> []

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (h, w) (Matrix cs _ _) = Matrix cs h w

row :: Int -> Matrix a -> Vector a
row x (Matrix cs _ w) = V.slice (w * x) w cs

shape :: Matrix a -> (Int, Int)
shape = rows &&& cols

transpose :: Matrix a -> Matrix a
transpose m@(Matrix _ _ w) = fromList $ map (toList . flip column m) [0..w - 1]
