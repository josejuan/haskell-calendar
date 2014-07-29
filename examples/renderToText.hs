{-# LANGUAGE OverloadedStrings #-}
import Data.Dates.Render.Calendar
import qualified Data.Text as T
import qualified Data.Time.Recurrence as R

renderMonthRules cell =
  case cell of
    VSeparator      -> " | "
    HSeparator      -> "---"
    MonthHeader d   -> T.center       21 ' ' $ T.pack $ show (toEnum (month d) :: R.Month) ++ " - " ++ show (year d)
    WeekDayHeader d -> T.justifyRight  3 ' ' $ T.pack $ take 2 $ show $ dateWeekDay d
    JustDay d       -> T.justifyRight  3 ' ' $ T.pack $ show $ day d
    EmptyDay        -> "   "

renderCalendarAsText cols rows year month weeday =
  renderCalendar sequence (putStrLn . T.unpack . T.concat . map renderMonthRules) cols rows year month weeday


{--

  $ ghci examples/renderToText.hs
  *Main> renderCalendarAsText 2 2 2014 8 Sunday

--}