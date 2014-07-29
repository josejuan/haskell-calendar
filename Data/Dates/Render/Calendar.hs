module Data.Dates.Render.Calendar (Cell(..), renderCalendar, module Data.Dates) where

import Data.Dates
import Data.List.Split

data Cell = VSeparator               -- Vertical separator
          | HSeparator               -- Horizontal separator
          | MonthHeader   DateTime   -- Month header (7 cells width)
          | WeekDayHeader DateTime   -- Week day header
          | JustDay       DateTime   -- Month day cell
          | EmptyDay                 -- Month empty cell
          deriving Show


makeMonthCalendar :: DateTime -> WeekDay -> [[Cell]]
makeMonthCalendar d weekday = [MonthHeader d] : weekDaysHeader : (chunksOf 7 $ take 42 $ monthDayA weekday)
  where weekDaysHeader = map WeekDayHeader $ take 7 $ dropWhile ((/=weekday).dateWeekDay) $ iterate (flip addInterval $ Days 1) d
        monthDayA wd | wd == dateWeekDay d = monthDayB d
                     | otherwise           = EmptyDay : monthDayA (intToWeekday (1 + (weekdayNumber wd) `mod` 7))
        monthDayB q  | month q == month d  = JustDay q : monthDayB (addInterval q (Days 1))
                     | otherwise           = repeat EmptyDay


makeCalendar :: Int -> Int -> DateTime -> WeekDay -> [[Cell]]
makeCalendar cols rows idate weekday =
  foldr1 (\a b -> a ++ s ++ b) $
  map (foldr1 $ \a b -> zipWith3 (\a b c -> a ++ b ++ c) a (repeat [VSeparator]) b) $
  chunksOf cols [makeMonthCalendar (addInterval idate (Months $ fromIntegral i)) weekday | i <- [0..cols * rows - 1]]
  where s = [take (8 * cols - 1) (repeat HSeparator)]


renderCalendar :: ([b] -> t) -> ([Cell] -> b) -> Int -> Int -> Int -> Int -> WeekDay -> t
renderCalendar joinRows renderCell cols rows year month weekday =
  joinRows $ map renderCell $ makeCalendar cols rows (DateTime year month 1 0 0 0) weekday
