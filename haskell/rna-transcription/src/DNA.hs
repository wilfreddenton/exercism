module DNA (toRNA) where

import Data.Maybe (isNothing, fromJust)

complement :: Char -> Maybe Char
complement 'G' = Just 'C'
complement 'C' = Just 'G'
complement 'T' = Just 'A'
complement 'A' = Just 'U'
complement _ = Nothing

toRNA :: String -> Maybe String
toRNA s = toRNA' s ""

toRNA' :: String -> String -> Maybe String
toRNA' "" o = Just o
toRNA' (x:xs) o =
  if isNothing c then Nothing else toRNA' xs $ o ++ [fromJust c]
  where c = complement x
