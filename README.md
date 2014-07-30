Simply use `renderCalendar` to generate a calendar providing *your own*
render function.

`renderCalendar` render all calendar cells from up/left to bottom/right:

    | cell   1, cell   2, cell   3, ... , cell j |
    | cell j+1, cell j+2, cell j+3, ...          |
    | ...                                        |

Each cell provider their render information (see `data Cell = ...`), for example:

    VSeparator

no information, simply a empty cell (external to every month)

    MonthHeader   DateTime

a month header (`(DateTime year month _ _ _ _)`), these cells are 7 wide spaced cells

and so on...


You can generate final calendar with something like

    renderCalendar               ::
      howToJoinRowsFunction      ([b] -> t) ->
      howToRenderCells           ([Cell] -> b) ->
      cols                       Int ->
      rows                       Int ->
      year                       Int ->
      month                      Int ->
      weeday                     WeekDay ->
                                 t

Example, to render a calendar to text you may define

    renderMonthRules cell =
      case cell of
        VSeparator      -> " | "
        HSeparator      -> "---"
        MonthHeader d   -> T.center       21 ' ' $ T.pack $ monthName[month d] ++ " - " ++ show (year d)
        WeekDayHeader d -> T.justifyRight  3 ' ' $ T.pack $ take 2 $ show $ dateWeekDay d
        JustDay d       -> T.justifyRight  3 ' ' $ T.pack $ show $ day d
        EmptyDay        -> "   "

then

    renderCalendar
        sequence
        (putStrLn . T.unpack . T.concat . map renderMonthRules)
        3 2 2014 8 Sunday

with resultant output

        August - 2014     |    September - 2014   |     October - 2014
     Su Mo Tu We Th Fr Sa |  Su Mo Tu We Th Fr Sa |  Su Mo Tu We Th Fr Sa
                     1  2 |      1  2  3  4  5  6 |            1  2  3  4
      3  4  5  6  7  8  9 |   7  8  9 10 11 12 13 |   5  6  7  8  9 10 11
     10 11 12 13 14 15 16 |  14 15 16 17 18 19 20 |  12 13 14 15 16 17 18
     17 18 19 20 21 22 23 |  21 22 23 24 25 26 27 |  19 20 21 22 23 24 25
     24 25 26 27 28 29 30 |  28 29 30             |  26 27 28 29 30 31
     31                   |                       |
    ---------------------------------------------------------------------
       November - 2014    |    December - 2014    |     January - 2015
     Su Mo Tu We Th Fr Sa |  Su Mo Tu We Th Fr Sa |  Su Mo Tu We Th Fr Sa
                        1 |      1  2  3  4  5  6 |               1  2  3
      2  3  4  5  6  7  8 |   7  8  9 10 11 12 13 |   4  5  6  7  8  9 10
      9 10 11 12 13 14 15 |  14 15 16 17 18 19 20 |  11 12 13 14 15 16 17
     16 17 18 19 20 21 22 |  21 22 23 24 25 26 27 |  18 19 20 21 22 23 24
     23 24 25 26 27 28 29 |  28 29 30 31          |  25 26 27 28 29 30 31
     30                   |                       |

Render to HTML can be achieved writing

    renderMonthRules cell =
      case cell of
        VSeparator      -> "<td class=s>&nbsp;</td>"
        HSeparator      -> "<td class=s>&nbsp;</td>"
        MonthHeader d   -> T.concat ["<td class=mh colspan=7>", T.pack $ monthName[month d] ++ " - " ++ show (year d), "</td>"]
        WeekDayHeader d -> T.concat ["<td class=wd>", T.pack $ take 4 $ show $ dateWeekDay d, "</td>"]
        JustDay d       -> T.concat ["<td class=dc>", T.pack $ show $ day d, "</td>"]
        EmptyDay        -> "<td class=ec>&nbsp;</td>"

    renderCalendarAsHtml cols rows year month weeday =
      renderCalendar T.concat (\cs -> T.concat ["<tr>", T.concat $ map renderMonthRules cs, "</tr>"]) cols rows year month weeday

with result

<img src="http://s9.postimg.org/vnx8o2y2n/calendar.png" />

Render to a graphical interface (e.g. <a href="https://hackage.haskell.org/package/gloss-1.8.2.1">Gloss</a>) can be achieved writing

    renderMonthRules cell =
      case cell of
        VSeparator      -> (1, Blank)
        HSeparator      -> (1, Blank)
        MonthHeader d   -> (7, Translate 3 0 $ dText 7 2 blue white $ show (toEnum (month d) :: R.Month) ++ " - " ++ show (year d))
        WeekDayHeader d -> (1, dText 1 0.3 orange white $ take 2 $ show $ dateWeekDay d)
        JustDay d       -> (1, dText 1 0.3 white black $ show $ day d)
        EmptyDay        -> (1, dText 1 0.3 (greyN 0.7) white "")

    dText w x c f t = Pictures [ Color (dark c) $ rectangleSolid w 1
                               , Color (light c) $ rectangleWire w 1
                               , Color f $ Translate (-x) (-0.25) $ scale 0.004 0.004 $ text t]

    foldPics f = Pictures . snd . foldl f (0, [])

    renderCalendarAsGloss cols rows year month weeday =
      renderCalendar (foldPics $ \(y, ps) xs -> (y - 1, Translate 0 y xs :ps))
                     (foldPics $ \(x, ps) c -> let (w, p) = renderMonthRules c in (x + w, Translate x 0 p :ps))
        cols rows year month weeday

with result

<img src="http://s9.postimg.org/vnx8o2y2n/calendar.png" />

(see `examples` folder).
















