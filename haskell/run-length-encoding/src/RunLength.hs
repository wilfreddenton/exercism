module RunLength (decode, encode) where

import Data.Char (isNumber)
import Data.List (takeWhile)

decode :: String -> String
decode "" = ""
decode s =
  let digits = takeWhile isNumber s
      len = length digits
      n = if null digits then 1 else read digits :: Int
  in replicate n (s!!len) ++ decode (drop (len + 1) s)

encode :: String -> String
encode "" = ""
encode s@(x:xs) =
  let n = length $ takeWhile (==x) s
  in if n == 1
     then x:encode xs
     else show n ++ x:encode (drop n s)
