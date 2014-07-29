{-# LANGUAGE OverloadedStrings #-}
import Data.Dates.Render.Calendar
import qualified Data.Text as T
import qualified Data.Time.Recurrence as R
import qualified Data.Text.IO as To

renderMonthRules cell =
  case cell of
    VSeparator      -> "<td class=s>&nbsp;</td>"
    HSeparator      -> "<td class=s>&nbsp;</td>"
    MonthHeader d   -> T.concat ["<td class=mh colspan=7>", T.pack $ show (toEnum (month d) :: R.Month) ++ " - " ++ show (year d), "</td>"]
    WeekDayHeader d -> T.concat ["<td class=wd>", T.pack $ take 4 $ show $ dateWeekDay d, "</td>"]
    JustDay d       -> T.concat ["<td class=dc>", T.pack $ show $ day d, "</td>"]
    EmptyDay        -> "<td class=ec>&nbsp;</td>"

renderCalendarAsHtml cols rows year month weeday =
  renderCalendar T.concat (\cs -> T.concat ["<tr>", T.concat $ map renderMonthRules cs, "</tr>"]) cols rows year month weeday

renderHtmlCalendarToFile fpath cols rows year month weeday =
  To.writeFile fpath $ T.concat [ htmlHeader, renderCalendarAsHtml cols rows year month weeday, htmlFooter ]

htmlHeader = "\
  \<!DOCTYPE html>                                                                                                                                \
  \<html>                                                                                                                                         \
  \  <head>                                                                                                                                       \
  \    <style>                                                                                                                                    \
  \     body { background-color: #f0f0a0 }                                                                                                        \
  \     .calendar { border-collapse: collapse; margin: auto }                                                                                     \
  \     .calendar td { border: 1px solid lightgray; text-align: right; background-color: white; padding: 0.2em }                                  \
  \     .calendar .mh { background-color: #0C93ED; color: white; text-align: center; border-radius: 22px 0 0; box-sizing: border-box; border: 0 }  \
  \     .calendar .wd { background-color: olive; color: white; font-size: 0.75em }                                                                \
  \     .calendar .ec { background-color: #f0f0f0 }                                                                                               \
  \     .calendar .s { background-color: #f0f0a0; border: 0; width: 2em; height: 2em; line-height: 0.5em; font-size: 0.5em }                      \
  \     .calendar tr .dc:nth-child(8n+7) { background-color: lightgreen }                                                                         \
  \    </style>                                                                                                                                   \
  \  </head>                                                                                                                                      \
  \  <body>                                                                                                                                       \
  \    <table class=calendar>                                                                                                                     \
  \"

htmlFooter = "\
  \    </table>                                                                                                                                   \
  \  </body>                                                                                                                                      \
  \</html>                                                                                                                                        \
  \"
