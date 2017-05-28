module Acronym (abbreviate) where

import Data.Char (toUpper, isUpper)
import Data.Maybe (isJust, fromJust)
import Data.List (findIndex)
import Data.List.Split (splitOneOf, splitOn)

abbreviate :: String -> String
abbreviate xs =
  let ws = splitOneOf " -" xs
  in map (toUpper . head) $ adjustOnFirstWord ws

adjustOnFirstWord :: [String] -> [String]
adjustOnFirstWord [] = []
adjustOnFirstWord ss@(w:ws)
  | w !! (length w - 1) == ':' = tail $ splitOn "" $ init w
  | isJust i && fromJust i /= 0 = splitAt' (succ $ fromJust i) w ++ ws
  | otherwise = ss
  where i = findIndex isUpper $ tail w

splitAt' :: Int -> String -> [String]
splitAt' n xs = [take n xs, drop n xs]
