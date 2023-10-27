module Data.Time.Ages.Time where

import Data.Time (NominalDiffTime, UTCTime(..), addUTCTime, diffUTCTime)
import Data.Time.Ages.Internal
import Data.Time.Clock (nominalDiffTimeToSeconds)

-- |
-- >>> addDurationToTime (UTCTime (fromGregorian 2023 10 25) 0) (PWeeks 2 <> PMinutes 17)
-- 2023-11-08 00:17:00 UTC
addDurationToTime :: UTCTime -> Duration -> UTCTime
addDurationToTime t d = addUTCTime (durationToNominal d) t

-- |
-- >>> durationToNominal (PMinutes 4 <> PSeconds 3 <> PMilliseconds 3)
-- 243.003s
durationToNominal :: Duration -> NominalDiffTime
durationToNominal (Duration p) = fromRational $ fromInteger p / factor Seconds

-- |
-- >>> durationPretty $ nominalToDuration 243.003
-- "4 mins 3 secs 3 millis"
nominalToDuration :: NominalDiffTime -> Duration
nominalToDuration = Duration . ceiling . (* factor Seconds) . toRational . nominalDiffTimeToSeconds

-- |
-- >>> durationPretty $ timeDiff (UTCTime (fromGregorian 2023 10 25) 0) (UTCTime (fromGregorian 2023 10 16) 500)
-- "1 week 1 day 23 hours 51 mins 40 secs"
-- >>> durationPretty $ timeDiff (UTCTime (fromGregorian 2023 10 15) 0) (UTCTime (fromGregorian 2023 10 16) 500)
-- "-1 day 8 mins 20 secs"
timeDiff :: UTCTime -> UTCTime -> Duration
timeDiff a b = nominalToDuration $ diffUTCTime a b
