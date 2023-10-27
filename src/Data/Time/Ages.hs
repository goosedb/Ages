{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Time.Ages (module E) where

import Data.Time.Ages.Internal as E (
  Duration,
  Unit (..),
  divDuration,
  mulDuration,
  sleep,
  timeout,
  toRaw,
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
