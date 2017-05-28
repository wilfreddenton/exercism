module Pangram (isPangram) where

import Data.Char (isLetter, toLower)
import Data.List (nub)

isPangram :: String -> Bool
isPangram = (==26) . length . nub . map toLower . filter isLetter
