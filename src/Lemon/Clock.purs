module Lemon.Clock where

import Data.Date (DayOfMonth(DayOfMonth), Date, now, Now)
import Data.Date.Locale (toLocaleTimeString, toLocaleDateString, Locale)
import Data.Date.UTC (minuteOfHour, month, hourOfDay, dayOfMonth)
import Data.Time (MinuteOfHour(MinuteOfHour), HourOfDay(HourOfDay))
import Lemon.Bar (Section)
import Prelude
import Prelude (show)
import Signal ((~>), unwrap)
import Signal.Time (second, every)

showDayOfMonth :: DayOfMonth -> String
showDayOfMonth (DayOfMonth i) = show (1 + i)

showHourOfDay :: HourOfDay -> String
showHourOfDay (HourOfDay i) = show i

showMinuteOfHour :: MinuteOfHour -> String
showMinuteOfHour (MinuteOfHour i) = show i

formatDate :: Date -> String
formatDate d =
  showDayOfMonth (dayOfMonth d) <> ". " <> show (month d) <> ". " <>
    showHourOfDay (hourOfDay d) <> ":" <> showMinuteOfHour (minuteOfHour d)

clockSection :: forall e. Section (now :: Now, locale :: Locale | e)
clockSection = unwrap $ every second ~> \_ -> do
  d <- now
  dateString <- toLocaleDateString d
  timeString <- toLocaleTimeString d
  pure (dateString <> " " <> timeString)
