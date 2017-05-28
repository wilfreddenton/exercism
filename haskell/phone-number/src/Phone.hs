module Phone (number) where

import Text.Regex.Posix

number :: String -> Maybe String
number xs =
  let matches = xs =~ "^((\\+?1)[ -.]*)?\\(?([2-9]{3})\\)?[ -.]*([2-9]{3})[ -.]*([0-9]{4})[ -.]*$" :: [[String]]
  in case length matches of
    1 -> Just $ concat $ drop (length m - 3) m where m = head matches
    _ -> Nothing
