{-# LANGUAGE OverloadedStrings #-}
import Data.Dates.Render.Calendar
import Graphics.Gloss
import qualified Data.Text as T
import qualified Data.Time.Recurrence as R

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

main = display (InWindow "Render calendar!" (1080, 1080) (0,0)) white
       (Translate (-19 * (3 * 8 - 1)) (19 * (3 * 9 - 1)) $ scale 38 38 $ renderCalendarAsGloss 3 3 2014 1 Monday)























