{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Time.Ages (module E) where

import Data.Time.Ages.Internal as E (
  Duration,
  Unit (..),
  divDuration,
  factor,
  mulDuration,
  sleep,
  timeout,
  toRaw,
  toRawInt,
  pattern PDays,
  pattern PHours,
  pattern PMicroseconds,
  pattern PMilliseconds,
  pattern PMinutes,
  pattern PNanoseconds,
  pattern PPicoseconds,
  pattern PSeconds,
  pattern PWeeks,
 )

import Data.Time.Ages.Pretty as E (
  durationPretty,
  durationPretty',
 )

import Data.Time.Ages.Time as E (
  addDurationToTime,
  durationToNominal,
  nominalToDuration,
  timeDiff
 )
