module DNA (nucleotideCounts) where

import Data.Map (Map, fromList, adjust)

nucleotides :: String
nucleotides = "ACGT"

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts [] = Right $ fromList [(x,0) | x <- nucleotides]
nucleotideCounts (x:xs)
  | x `elem` nucleotides = adjust succ x <$> nucleotideCounts xs
  | otherwise = Left "String is not DNA"
