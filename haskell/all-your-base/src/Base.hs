module Base (rebase) where

rebase :: (Integral a, Show a, Read a) => a -> a -> [a] -> Maybe [a]
rebase inputBase outputBase inputDigits
  | inputBase <= 1 || outputBase <= 1 || any (\d -> d < 0 || d >= inputBase) inputDigits = Nothing
  | otherwise = Just $ encode outputBase $ decode inputBase inputDigits

decode :: Integral a => a -> [a] -> a
decode _ [] = 0
decode b (d:ds) = (d * b ^ length ds) + decode b ds

encode :: Integral a => a -> a -> [a]
encode _ 0 = []
encode base n = encode base q ++ [r]
  where r = n `mod` base
        q = n `div` base
