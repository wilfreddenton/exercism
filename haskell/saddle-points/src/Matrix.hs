module Matrix (saddlePoints) where

import Data.Array (Array, bounds, (!))

type Point a = (a,a)

saddlePoints :: Array (Point Int) Int -> [Point Int]
saddlePoints matrix = filter isSaddlePoint [(x,y) | x <- xRange, y <- yRange]
  where (_, (h,w)) = bounds matrix
        xRange = [0..h]
        yRange = [0..w]
        rowMaxes = [maximum [matrix ! (x,y) | y <- yRange] | x <- xRange]
        colMins = [minimum [matrix ! (x,y) | x <- xRange] | y <- yRange]
        isSaddlePoint p@(x,y) = maxX == minY && minY == matrix ! p
          where maxX = rowMaxes !! x
                minY = colMins !! y
