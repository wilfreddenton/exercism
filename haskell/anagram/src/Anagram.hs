module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter (isAnagram (map toLower xs) . map toLower)
  where isAnagram s t = s /= t && sort s == sort t
