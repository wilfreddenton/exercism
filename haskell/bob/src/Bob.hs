module Bob (responseFor) where

import Data.Char (isSpace)
import Text.Regex.Posix

responseFor :: String -> String
responseFor s = responseFor' $ filter (not . isSpace) s

responseFor' :: String -> String
responseFor' "" = "Fine. Be that way!"
responseFor' s
  | isYell s = "Whoa, chill out!"
  | last s == '?' = "Sure."
  | otherwise = "Whatever."

isYell :: String -> Bool
isYell s = (s =~ "^[A-Z0-9!@#$%^&*()_+?,]+$" :: Bool) && not (s =~ "^[0-9,?]+$" :: Bool)
