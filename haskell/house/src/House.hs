module House (rhyme) where

import Text.Printf (printf)

nounVerbPairs :: [(String, String)]
nounVerbPairs = [("house that Jack built", ""), ("malt", "lay in"), ("rat", "ate"), ("cat", "killed")
                ,("dog", "worried"), ("cow with the crumpled horn", "tossed"), ("maiden all forlorn", "milked"), ("man all tattered and torn", "kissed")
                ,("priest all shaven and shorn", "married"), ("rooster that crowed in the morn", "woke"), ("farmer sowing his corn", "kept"), ("horse and the hound and the horn", "belonged to")]

rhyme :: String
rhyme = init $ concatMap (generate nounVerbPairs) [0..length nounVerbPairs - 1]

generate :: [(String, String)] -> Int -> String
generate ((n,_):_) 0 = printf "This is the %s.\n\n" n
generate xs i = printf "This is the %s\nthat %s" n v ++ embed (take i xs)
  where (n,v) = xs !! i

embed :: [(String, String)] -> String
embed [(n,_)] = printf " the %s.\n\n" n
embed xs = printf " the %s\nthat %s" n v ++ embed (init xs)
  where (n,v) = last xs
