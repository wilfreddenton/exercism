module SecretHandshake (handshake) where

handshake :: Int -> [String]
handshake i = handshake' (i `mod` 32) []
  where
    handshake' n xs
      | n >= 16 = reverse $ handshake' (n - 16) xs
      | n >= 8 = handshake' (n - 8) $ "jump":xs
      | n >= 4 = handshake' (n - 4) $ "close your eyes":xs
      | n >= 2 = handshake' (n - 2) $ "double blink":xs
      | n == 1 = "wink":xs
      | otherwise = xs
