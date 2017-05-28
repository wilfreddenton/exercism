module Beer (song) where

import Data.Char (toUpper)

one :: String
one = " bottle"

mult :: String
mult = one ++ "s"

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

line1 :: Int -> String
line1 n
  | n == 0 = capitalize zero ++ a ++ zero ++ b
  | n == 1 = ns ++ one ++ a ++ ns ++ one ++ b
  | otherwise = ns ++ mult ++ a ++ ns ++ mult ++ b
  where zero = "no more" ++ mult
        b = " of beer."
        a = " of beer on the wall, "
        ns = show n

line2 :: Int -> String
line2 n
  | n == 0 = "Go to the store and buy some more, 99" ++ mult ++ b
  | n == 2 = a ++ "1" ++ one ++ b
  | n == 1 = a ++ zero ++ b
  | otherwise = a ++ show (n - 1) ++ mult ++ b
  where zero = "no more" ++ mult
        a = "Take " ++ (if n == 1 then "it" else "one") ++ " down and pass it around, "
        b = " of beer on the wall."

verse :: Int -> String
verse n = line1 n ++ "\n" ++ line2 n ++ if n /= 0 then "\n\n" else "\n"

song :: String
song = concatMap verse [99,98..0]
