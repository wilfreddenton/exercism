module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, addDays)
import Data.Time.Calendar.WeekDate (toWeekDate)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Enum)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth
              deriving (Enum)

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = addDays (diff (toWeekDay startDay) weekday) startDay
  where fromGregorian' = fromGregorian year month
        startDay = case schedule of
          Teenth -> fromGregorian' 13
          Last -> addDays (-6) $ fromGregorian' 31
          _ -> fromGregorian' $ 1 + fromEnum schedule * 7

toWeekDay :: Day -> Weekday
toWeekDay d = toEnum $ dayOfWeek - 1
  where (_,_,dayOfWeek) = toWeekDate d

diff :: Weekday -> Weekday -> Integer
diff start end = toInteger $ mod (fromEnum end - fromEnum start) 7
