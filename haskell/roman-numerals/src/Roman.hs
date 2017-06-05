module Roman (numerals) where

toNumeral :: Char -> Char -> Char -> Char -> String
toNumeral digit one five ten
  | digit == '1' = [one]
  | digit `elem` ['2','3'] = replicate (read [digit]) one
  | digit == '4' = [one, five]
  | digit == '5' = [five]
  | digit `elem` ['6', '7', '8'] = five : replicate (read [digit] - 5) one
  | otherwise = [one, ten]

numerals :: Integer -> Maybe String
numerals n
  | n > 3000 || n < 1 = Nothing
  | otherwise = Just $ numerals' $ show n

numerals' :: String -> String
numerals' [] = []
numerals' (x:xs)
  | x == '0' = numerals' xs
  | otherwise = chars ++ numerals' xs
  where chars = case length xs of
                  3 -> toNumeral x 'M' '_' '_'
                  2 -> toNumeral x 'C' 'D' 'M'
                  1 -> toNumeral x 'X' 'L' 'C'
                  _ -> toNumeral x 'I' 'V' 'X'
