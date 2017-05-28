module ETL (transform) where

import Data.Map (Map, toList, fromList)
import Data.Char (toLower)

transform :: Map a String -> Map Char a
transform legacyData = fromList [(toLower x, v) | (v, xs) <- toList legacyData, x <- xs]
