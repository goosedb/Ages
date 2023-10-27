module Data.Time.Ages.Time where

import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Ages.Internal
import Data.Time.Clock (nominalDiffTimeToSeconds)

addDurationToTime :: UTCTime -> Duration -> UTCTime
addDurationToTime t d = addUTCTime (durationToNominal d) t

durationToNominal :: Duration -> NominalDiffTime
durationToNominal (Duration p) = fromRational $ fromInteger p / factor Seconds

nominalToDuration :: NominalDiffTime -> Duration
nominalToDuration = Duration . ceiling . (* factor Seconds) . toRational . nominalDiffTimeToSeconds

timeDiff :: UTCTime -> UTCTime -> Duration
timeDiff a b = nominalToDuration $ diffUTCTime a b
