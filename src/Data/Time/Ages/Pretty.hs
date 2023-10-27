{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Time.Ages.Pretty where

import Data.Foldable (Foldable (..))
import Data.Time.Ages.Internal
import GHC.Integer (quotRemInteger)

data UnitWord = UnitWord {singular :: String, plural :: String}
  deriving (Show, Read, Eq, Ord)

{- |
>>> durationPretty (PMinutes 4 <> PSeconds 68 <> PHours 25)
"1 day 1 hour 5 mins 8 secs"
-}
durationPretty :: Duration -> String
durationPretty = durationPretty' Picoseconds defaultUnitWords

defaultUnitWords :: Unit -> UnitWord
defaultUnitWords = \case
  Picoseconds -> mkuw "pico"
  Nanoseconds -> mkuw "nano"
  Microseconds -> mkuw "micro"
  Milliseconds -> mkuw "milli"
  Seconds -> mkuw "sec"
  Minutes -> mkuw "min"
  Hours -> mkuw "hour"
  Days -> mkuw "day"
  Weeks -> mkuw "week"
 where
  mkuw s = UnitWord s (s <> "s")

{- |
>>> durationPretty' Seconds defaultUnitWords (PMinutes 4 <> PSeconds 68 <> PHours 25)
"1 day 1 hour 5 mins 8 secs"
>>> durationPretty' Minutes defaultUnitWords (PMinutes 4 <> PSeconds 68 <> PHours 25)
"1 day 1 hour 5 mins"
>>> durationPretty' Minutes (\u -> if u == Minutes then UnitWord "minute" "minutes" else defaultUnitWords u) (PMinutes 4 <> PSeconds 68 <> PHours 25)
"1 day 1 hour 5 minutes"
-}
durationPretty' ::
  -- | Minimal unit to display
  Unit ->
  (Unit -> UnitWord) ->
  Duration ->
  String
durationPretty' minUnit toStr (Duration p) =
  applySign
    . unwords
    . ifEmpty
    . reverse
    . fst
    $ foldl'
      do
        \(sacc, pacc) (unit, uw) ->
          if pacc >= factor unit
            then
              let (# i, r #) = quotRemInteger pacc (factor unit)
                  istr = [show i <> " " <> applyUnitWord i uw | i /= 0]
               in (istr <> sacc, r)
            else (sacc, pacc)
      do ([], abs p)
      do list
 where
  applySign = if p >= 0 then id else ("-" <>)
  applyUnitWord x UnitWord{..} = if x == 1 then singular else plural
  list = map wrap $ reverse [minUnit .. maxBound]
  ifEmpty = \case
    [] -> ["0 " <> plural (toStr minUnit)]
    a -> a
  wrap u = (u, toStr u)
