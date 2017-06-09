module Clock (clockHour, clockMin, fromHourMin, toString) where

import Text.Printf (printf)

newtype Clock = Clock Int deriving (Eq)

instance Show Clock where
  show = toString

instance Num Clock where
  fromInteger n = fromHourMin 0 $ fromInteger n
  (Clock s1) + (Clock s2) = fromInteger $ fromIntegral $ s1 + s2
  _ * _ = undefined
  signum (Clock 0) = 0
  signum _ = 1
  abs c = c
  negate c = fromHourMin (-h) (-m)
    where h = clockHour c
          m = clockMin c

clockHour :: Clock -> Int
clockHour (Clock s) = s `div` 60

clockMin :: Clock -> Int
clockMin (Clock s) = s `mod` 60

fromHourMin :: Int -> Int -> Clock
fromHourMin hour minutes = Clock (h * 60 + r)
  where (q, r) = divMod minutes 60
        h = (hour + q) `mod` 24

toString :: Clock -> String
toString clock = printf "%02d:%02d" (clockHour clock) (clockMin clock)
