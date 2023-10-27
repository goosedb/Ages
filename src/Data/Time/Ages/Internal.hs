{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Time.Ages.Internal where

import qualified Control.Concurrent as Conc
import Data.List (dropWhileEnd, find, foldl')
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import GHC.Integer (quotRemInteger)
import qualified System.Timeout as Conc

newtype Duration = Duration {getPicos :: Integer}
  deriving (Show, Read, Eq, Ord)

instance Semigroup Duration where
  Duration a <> Duration b = Duration (a + b)

instance Monoid Duration where
  mempty = Duration 0

data Unit
  = Picoseconds
  | Nanoseconds
  | Microseconds
  | Milliseconds
  | Seconds
  | Minutes
  | Hours
  | Days
  | Weeks
  deriving (Enum, Bounded, Eq, Ord, Show, Read)

pattern PPicoseconds :: Integer -> Duration
pattern PPicoseconds n = Duration n

pattern PNanoseconds :: Integer -> Duration
pattern PNanoseconds n <- (divDuration (factor Nanoseconds) -> Duration n)
  where
    PNanoseconds n = Duration (n * factor Nanoseconds)

pattern PMicroseconds :: Integer -> Duration
pattern PMicroseconds n <- (divDuration (factor Microseconds) -> Duration n)
  where
    PMicroseconds n = Duration (n * factor Microseconds)

pattern PMilliseconds :: Integer -> Duration
pattern PMilliseconds n <- (divDuration (factor Milliseconds) -> Duration n)
  where
    PMilliseconds n = Duration (n * factor Milliseconds)

pattern PSeconds :: Integer -> Duration
pattern PSeconds n <- (divDuration (factor Seconds) -> Duration n)
  where
    PSeconds n = Duration (n * factor Seconds)

pattern PMinutes :: Integer -> Duration
pattern PMinutes n <- (divDuration (factor Minutes) -> Duration n)
  where
    PMinutes n = Duration (n * factor Minutes)

pattern PHours :: Integer -> Duration
pattern PHours n <- (divDuration (factor Hours) -> Duration n)
  where
    PHours n = Duration (n * factor Hours)

pattern PDays :: Integer -> Duration
pattern PDays n <- (divDuration (factor Days) -> Duration n)
  where
    PDays n = Duration (n * factor Days)

pattern PWeeks :: Integer -> Duration
pattern PWeeks n <- (divDuration (factor Days) -> Duration n)
  where
    PWeeks n = Duration (n * factor Weeks)

nanoFactor :: Integer
nanoFactor = 10e3

microFactor :: Integer
microFactor = 10e6

milliFactor :: Integer
milliFactor = 10e9

secondFactor :: Integer
secondFactor = 10e12

minuteFactor :: Integer
minuteFactor = secondFactor * 60

hourFactor :: Integer
hourFactor = minuteFactor * 60

dayFactor :: Integer
dayFactor = hourFactor * 24

weekFactor :: Integer
weekFactor = dayFactor * 7

factor :: (Num a) => Unit -> a
factor =
  fromInteger . \case
    Picoseconds -> 1
    Nanoseconds -> nanoFactor
    Microseconds -> microFactor
    Milliseconds -> milliFactor
    Seconds -> secondFactor
    Minutes -> minuteFactor
    Hours -> hourFactor
    Days -> dayFactor
    Weeks -> weekFactor

divDuration :: Integer -> Duration -> Duration
divDuration n (Duration x) = Duration (x `div` n)

mulDuration :: Integer -> Duration -> Duration
mulDuration n (Duration x) = Duration (x * n)

toRaw :: Duration -> Unit -> Integer
d `toRaw` u = fromInteger case u of
  Picoseconds -> let PPicoseconds x = d in x
  Nanoseconds -> let PNanoseconds x = d in x
  Microseconds -> let PMicroseconds x = d in x
  Milliseconds -> let PMilliseconds x = d in x
  Seconds -> let PSeconds x = d in x
  Minutes -> let PMinutes x = d in x
  Hours -> let PHours x = d in x
  Days -> let PDays x = d in x

sleep :: Duration -> IO ()
sleep (PMicroseconds ms) = Conc.threadDelay (fromInteger ms)

timeout :: Duration -> IO a -> IO (Maybe a)
timeout (PMicroseconds ms) = Conc.timeout (fromInteger ms)
